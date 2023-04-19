(**
    (** [pow x y] returns [x^y] *)
let rec two_pow x = if x = 0 then 1 else 2 * two_pow (x - 1)

(** Get a list of the names of all Propositions in an expression *)
let rec get_props (e : Command.expression) : char list =
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
let rec eval_prop (prop_vals : (char * bool) list) (e : Command.expression) :
    bool =
  match e with
  | Prop c -> List.assoc c prop_vals
  | Dis (e1, e2) -> eval_prop prop_vals e1 || eval_prop prop_vals e2
  | Conj (e1, e2) -> eval_prop prop_vals e1 && eval_prop prop_vals e2
  | Impl (e1, e2) -> (not (eval_prop prop_vals e1)) || eval_prop prop_vals e2
  | Bi (e1, e2) -> eval_prop prop_vals e1 = eval_prop prop_vals e2
  | Neg e1 -> not (eval_prop prop_vals e1)

(** Returns a list of tuples of propositions, booleans by assigning each
    proposition a truth value based on the configuration. The proposition which
    is at position i in props will be true if and only if the ith digit of the
    binary representation of config is a 1. *)
let rec prop_vals_from_config (props : char list) (config : int) :
    (char * bool) list =
  match props with
  | h :: t ->
      if config mod 2 = 1 then
        (h, true) :: prop_vals_from_config t ((config - 1) / 2)
      else (h, false) :: prop_vals_from_config t (config / 2)
  | [] -> []

(** Compares two propositions for the case specified by config *)
let compare_case (e1 : Command.expression) (e2 : Command.expression)
    (props : char list) (config : int) : bool =
  let prop_vals = prop_vals_from_config props config in
  eval_prop prop_vals e1 = eval_prop prop_vals e2

(** Returns: true if and only if e1 and e2 have the same truth value for the
    case specified by config and for all cases specified by a lower-number
    config*)
let rec compare_rec (e1 : Command.expression) (e2 : Command.expression)
    (props : char list) (config : int) : bool =
  if not (compare_case e1 e2 props config) then false
  else if config > 0 then compare_rec e1 e2 props (config - 1)
  else true

let compare (e1 : Command.expression) (e2 : Command.expression) =
  let props = get_props e1 @ get_props e2 |> List.sort_uniq Stdlib.compare in
  compare_rec e1 e2 props (two_pow (List.length props) - 1)
  *)
