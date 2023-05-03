type logic_expression =
  | Prop of char
  | Dis of logic_expression * logic_expression
  | Conj of logic_expression * logic_expression
  | Impl of logic_expression * logic_expression
  | Bi of logic_expression * logic_expression
  | Neg of logic_expression

type set_expression =
  | Set of char
  | Comp of set_expression
  | Intersection of set_expression * set_expression
  | Union of set_expression * set_expression
  | Difference of set_expression * set_expression

exception Empty
exception Malformed

(**This converts a string into a list of its chars*)
let explode str =
  let rec explode_help i lst =
    if i = 0 then lst else explode_help (i - 1) (str.[i - 1] :: lst)
  in
  explode_help (String.length str) []

(*propositions and set names must be capital letters*)
let is_capital ascii = ascii >= 65 && ascii <= 90

(*This removes as many parentheses from the outside of an expression
  (represented as a char list) as possible*)
let rec peel_parentheses lst =
  let rec can_peel lst cnt =
    if cnt = -1 then
      match lst with
      | [] -> true
      | _ -> false
    else
      match lst with
      | '(' :: t -> can_peel t (cnt + 1)
      | ')' :: t -> can_peel t (cnt - 1)
      | _ :: t -> can_peel t cnt
      | [] -> false
  in
  match lst with
  | '(' :: t ->
      if can_peel t 0 then
        match List.rev t with
        | ')' :: t -> peel_parentheses (List.rev t)
        | _ -> failwith "Shouldn't happen" [@coverage off]
      else lst
  | _ -> lst

(*to shorten code - matches all operations represented by a single character*)
let rec match_singles_logic lst cnt acc find_operand get_expr =
  match lst with
  | '(' :: t -> find_operand t (acc @ [ '(' ]) (cnt + 1)
  | ')' :: t -> find_operand t (acc @ [ ')' ]) (cnt - 1)
  | 'v' :: t ->
      if cnt = 0 then Dis (get_expr acc, get_expr t)
      else find_operand t (acc @ [ 'v' ]) cnt
  | '^' :: t ->
      if cnt = 0 then Conj (get_expr acc, get_expr t)
      else find_operand t (acc @ [ '^' ]) cnt
  | '!' :: t ->
      if cnt = 0 then if acc = [] then Neg (get_expr t) else raise Malformed
      else find_operand t (acc @ [ '!' ]) cnt
  | _ -> failwith "never happens" [@coverage off]

(*Finds the next operand in the char list and processes it*)
and find_operand_logic lst acc cnt =
  if cnt < 0 then raise Malformed
  else
    let find_operand = find_operand_logic in
    let get_expr = logic_expression_from_char_list in
    match lst with
    | '(' :: _ | ')' :: _ | 'v' :: _ | '^' :: _ | '!' :: _ ->
        match_singles_logic lst cnt acc find_operand get_expr
    | '=' :: '>' :: t ->
        if cnt = 0 then Impl (get_expr acc, get_expr t)
        else find_operand t (acc @ ('=' :: [ '>' ])) cnt
    | '<' :: '=' :: '>' :: t ->
        if cnt = 0 then Bi (get_expr acc, get_expr t)
        else find_operand t (acc @ ('<' :: '=' :: [ '>' ])) cnt
    | [ a ] ->
        if acc = [] && a |> Char.code |> is_capital then Prop a
        else raise Malformed
    | h :: t -> find_operand t (acc @ [ h ]) cnt
    | [] -> raise Malformed

(*A helper function to allow for mutual recursion between
  [logic_expression_from_char_list] and [find_operand_logic]*)
and logic_expression_from_char_list lst =
  let lst = peel_parentheses lst in
  find_operand_logic lst [] 0

(*creates an expression from a string list as a helper function for
  parse_logic*)
let expression_from_logic str =
  let exploded_str = List.fold_left (fun acc x -> acc @ explode x) [] str in
  let spaces_removed_rev =
    List.fold_left
      (fun acc x -> if x <> ' ' then x :: acc else acc)
      [] exploded_str
  in
  let spaces_removed = List.rev spaces_removed_rev in
  logic_expression_from_char_list spaces_removed

let parse_logic (lst : string list) =
  match lst with
  | [] -> raise Empty
  | _ -> expression_from_logic lst

let rec string_of_logic_expr expr =
  match expr with
  | Prop a -> "(" ^ String.make 1 a ^ ")"
  | Dis (a, b) ->
      "(" ^ string_of_logic_expr a ^ "v" ^ string_of_logic_expr b ^ ")"
  | Conj (a, b) ->
      "(" ^ string_of_logic_expr a ^ "^" ^ string_of_logic_expr b ^ ")"
  | Impl (a, b) ->
      "(" ^ string_of_logic_expr a ^ "=>" ^ string_of_logic_expr b ^ ")"
  | Bi (a, b) ->
      "(" ^ string_of_logic_expr a ^ "<=>" ^ string_of_logic_expr b ^ ")"
  | Neg a -> "(" ^ "!" ^ string_of_logic_expr a ^ ")"

(*matches the operators represented by a single character*)
let rec match_singles_set lst cnt acc find_operand get_expr =
  match lst with
  | '(' :: t -> find_operand t (acc @ [ '(' ]) (cnt + 1)
  | ')' :: t -> find_operand t (acc @ [ ')' ]) (cnt - 1)
  | 'v' :: t ->
      if cnt = 0 then Union (get_expr acc, get_expr t)
      else find_operand t (acc @ [ 'v' ]) cnt
  | '^' :: t ->
      if cnt = 0 then Intersection (get_expr acc, get_expr t)
      else find_operand t (acc @ [ '^' ]) cnt
  | '\\' :: t ->
      if cnt = 0 then Difference (get_expr acc, get_expr t)
      else find_operand t (acc @ [ '\\' ]) cnt
  | _ -> failwith "never happens" [@coverage off]

(*Finds the next operand in the char list and processes it*)
and find_operand_set lst acc cnt =
  if cnt < 0 then raise Malformed
  else
    let find_operand = find_operand_set in
    let get_expr = set_expression_from_char_list in
    match lst with
    | '(' :: _ | ')' :: _ | 'v' :: _ | '^' :: _ | '\\' :: _ ->
        match_singles_set lst cnt acc find_operand get_expr
    | 'C' :: 'o' :: 'm' :: 'p' :: t ->
        if cnt = 0 then if acc = [] then Comp (get_expr t) else raise Malformed
        else find_operand t (acc @ ('C' :: 'o' :: 'm' :: [ 'p' ])) cnt
    | [ a ] ->
        if acc = [] && a |> Char.code |> is_capital then Set a
        else raise Malformed
    | h :: t -> find_operand t (acc @ [ h ]) cnt
    | [] -> raise Malformed

(*A helper function to allow for mutual recursion between
  [set_expression_from_char_list] and [find_operand_set]*)
and set_expression_from_char_list lst =
  let lst = peel_parentheses lst in
  find_operand_set lst [] 0

(*creates an expression from a string list as helper function for parse_set*)
let expression_from_set str =
  let exploded_str = List.fold_left (fun acc x -> acc @ explode x) [] str in
  let spaces_removed_rev =
    List.fold_left
      (fun acc x -> if x <> ' ' then x :: acc else acc)
      [] exploded_str
  in
  let spaces_removed = List.rev spaces_removed_rev in
  set_expression_from_char_list spaces_removed

let parse_set (lst : string list) =
  match lst with
  | [] -> raise Empty
  | _ -> expression_from_set lst

let rec string_of_set_expr expr =
  match expr with
  | Set a -> "(" ^ String.make 1 a ^ ")"
  | Union (a, b) ->
      "(" ^ string_of_set_expr a ^ "v" ^ string_of_set_expr b ^ ")"
  | Intersection (a, b) ->
      "(" ^ string_of_set_expr a ^ "^" ^ string_of_set_expr b ^ ")"
  | Difference (a, b) ->
      "(" ^ string_of_set_expr a ^ "\\" ^ string_of_set_expr b ^ ")"
  | Comp a -> "(" ^ "Comp" ^ string_of_set_expr a ^ ")"
