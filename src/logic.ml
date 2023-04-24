(** [two_pow x] returns [2^x] *)
let rec two_pow x = if x = 0 then 1 else 2 * two_pow (x - 1)

(** Get a list of the names of all Propositions in a logic_expression *)
let rec get_props (e : Command.logic_expression) : char list =
  match e with
  | Prop c -> [ c ]
  | Dis (e1, e2) -> get_props e1 @ get_props e2
  | Conj (e1, e2) -> get_props e1 @ get_props e2
  | Impl (e1, e2) -> get_props e1 @ get_props e2
  | Bi (e1, e2) -> get_props e1 @ get_props e2
  | Neg e1 -> get_props e1

(** Determines if an expression is true or false, given the truth values of all
    it's propositions. Requires: [prop_vals] contains a [prop, value] pair for
    every prop in [get_props e]*)
let rec eval_prop (prop_vals : (char * bool) list)
    (e : Command.logic_expression) : bool =
  match e with
  | Prop c -> List.assoc c prop_vals
  | Dis (e1, e2) -> eval_prop prop_vals e1 || eval_prop prop_vals e2
  | Conj (e1, e2) -> eval_prop prop_vals e1 && eval_prop prop_vals e2
  | Impl (e1, e2) -> (not (eval_prop prop_vals e1)) || eval_prop prop_vals e2
  | Bi (e1, e2) -> eval_prop prop_vals e1 = eval_prop prop_vals e2
  | Neg e1 -> not (eval_prop prop_vals e1)

(** Returns a list of tuples of chars, booleans by assigning each proposition a
    truth value based on the configuration. The char (proposition or set) which
    is at position i in props will be paired with true if the ith digit of the
    binary representation of config is a 1, and will be paired with false
    otherwise. *)
let rec vals_from_config (chars : char list) (config : int) : (char * bool) list
    =
  match chars with
  | h :: t ->
      if config mod 2 = 1 then (h, true) :: vals_from_config t ((config - 1) / 2)
      else (h, false) :: vals_from_config t (config / 2)
  | [] -> []

(** Returns: true if and only if [e1] is [false] or [e2] is [true] for the
    propositions with truth values as given in [props] *)
let compare_logic_case (e1 : Command.logic_expression)
    (e2 : Command.logic_expression) (props : char list) (config : int) : bool =
  let prop_vals = vals_from_config props config in
  (not (eval_prop prop_vals e1)) || eval_prop prop_vals e2

(** Returns: true if and only if [compare_logic_case e1 e2 num] is true for all
    0 <= num <= config *)
let rec compare_logic_rec (e1 : Command.logic_expression)
    (e2 : Command.logic_expression) (props : char list) (config : int) : bool =
  if not (compare_logic_case e1 e2 props config) then false
  else if config > 0 then compare_logic_rec e1 e2 props (config - 1)
  else true

(** Returns: true if and only if [e1] is [false] or [e2] is [true] for all
    possible configurations of truth values for the propositions*)
let compare_logic (e1 : Command.logic_expression)
    (e2 : Command.logic_expression) =
  let props = get_props e1 @ get_props e2 |> List.sort_uniq Stdlib.compare in
  compare_logic_rec e1 e2 props (two_pow (List.length props) - 1)

(** Get a list of the sets in a set_expression *)
let rec get_sets (e : Command.set_expression) : char list =
  match e with
  | Set s -> [ s ]
  | Comp s -> get_sets s
  | Intersection (s1, s2) -> get_sets s1 @ get_sets s2
  | Union (s1, s2) -> get_sets s1 @ get_sets s2
  | Difference (s1, s2) -> get_sets s1 @ get_sets s2

(** Determines if an element, which is a member of a set [A] if the pair
    [A, true] is in [set_vals] and is not if [A, false] is in [set_vals], is a
    member of the set represented by the expression [e] or not . Requires:
    [set_vals] contains a [prop, value] pair for every set in [get_sets s]*)
let rec member_of_set (set_vals : (char * bool) list)
    (e : Command.set_expression) : bool =
  match e with
  | Set s -> List.assoc s set_vals
  | Comp e1 -> not (member_of_set set_vals e1)
  | Union (e1, e2) -> member_of_set set_vals e1 || member_of_set set_vals e2
  | Intersection (e1, e2) ->
      member_of_set set_vals e1 && member_of_set set_vals e2
  | Difference (e1, e2) ->
      member_of_set set_vals e1 && not (member_of_set set_vals e2)

(** Returns true if and only if an element of the sets in [sets] that are
    described by [true] in [config], is not in [e1] or is in [e2] *)
let compare_set_case (e1 : Command.set_expression) (e2 : Command.set_expression)
    (sets : char list) (config : int) : bool =
  let set_vals = vals_from_config sets config in
  (not (member_of_set set_vals e1)) || member_of_set set_vals e2

(** Returns: true if and only if [compare_set_case e1 e2 num] is true for all 0
    <= num <= config *)
let rec compare_set_rec (e1 : Command.set_expression)
    (e2 : Command.set_expression) (props : char list) (config : int) : bool =
  if not (compare_set_case e1 e2 props config) then false
  else if config > 0 then compare_set_rec e1 e2 props (config - 1)
  else true

(** Returns: true if and only if an element which is in some primary sets and
    not in others, for all possible configurations of being in some sets and not
    in others, is not a member of the set [e1] or is a member of the set [e2] *)
let compare_sets (e1 : Command.set_expression) (e2 : Command.set_expression) =
  let sets = get_sets e1 @ get_sets e2 |> List.sort_uniq Stdlib.compare in
  compare_set_rec e1 e2 sets (two_pow (List.length sets) - 1)
