type expression =
  | Prop of char
  | Dis of expression * expression
  | Conj of expression * expression
  | Impl of expression * expression
  | Bi of expression * expression
  | Neg of expression

exception Empty
exception Malformed

let explode str =
  let rec explode_help i lst =
    if i = 0 then lst else explode_help (i - 1) (str.[i - 1] :: lst)
  in
  explode_help (String.length str) []

(*propositions must be capital letters*)
let is_proposition ascii = ascii >= 65 && ascii <= 90

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
        | _ -> failwith "Shouldn't happen"
      else lst
  | _ -> lst

let rec expression_from_char_list lst =
  let lst = peel_parentheses lst in
  (* let _ = print_endline (String.concat " " (List.map (String.make 1) lst))
     in *)
  let rec find_operand lst acc cnt =
    if cnt < 0 then
      let _ = print_endline "4" in
      raise Malformed
    else
      match lst with
      | '(' :: t -> find_operand t (acc @ [ '(' ]) (cnt + 1)
      | ')' :: t -> find_operand t (acc @ [ ')' ]) (cnt - 1)
      | 'v' :: t ->
          if cnt = 0 then
            Dis (expression_from_char_list acc, expression_from_char_list t)
          else find_operand t (acc @ [ 'V' ]) cnt
      | '^' :: t ->
          if cnt = 0 then
            Conj (expression_from_char_list acc, expression_from_char_list t)
          else find_operand t (acc @ [ '^' ]) cnt
      | '=' :: '>' :: t ->
          if cnt = 0 then
            Impl (expression_from_char_list acc, expression_from_char_list t)
          else find_operand t (acc @ ('=' :: [ '>' ])) cnt
      | '<' :: '=' :: '>' :: t ->
          if cnt = 0 then
            Bi (expression_from_char_list acc, expression_from_char_list t)
          else find_operand t (acc @ ('<' :: '=' :: [ '>' ])) cnt
      | [ a ] ->
          if acc = [] then
            if a |> Char.code |> is_proposition then Prop a else raise Malformed
          else raise Malformed
      | '!' :: t ->
          if cnt = 0 then Neg (expression_from_char_list t)
          else find_operand t (acc @ [ '!' ]) cnt
      | h :: t -> find_operand t (acc @ [ h ]) cnt
      | [] -> raise Malformed
  in
  match lst with
  | [ a ] ->
      if a |> Char.code |> is_proposition then Prop a else raise Malformed
  | '!' :: t -> Neg (expression_from_char_list t)
  | _ -> find_operand lst [] 0

let expression_from_logic str =
  let exploded_str = List.fold_left (fun acc x -> acc @ explode x) [] str in
  let spaces_removed_rev =
    List.fold_left
      (fun acc x -> if x <> ' ' then x :: acc else acc)
      [] exploded_str
  in
  let spaces_removed = List.rev spaces_removed_rev in
  expression_from_char_list spaces_removed

let parse (lst : string list) =
  match lst with
  | [] -> raise Empty
  | _ -> expression_from_logic lst

let rec string_of_expr expr =
  match expr with
  | Prop a -> "(" ^ String.make 1 a ^ ")"
  | Dis (a, b) -> "(" ^ string_of_expr a ^ "v" ^ string_of_expr b ^ ")"
  | Conj (a, b) -> "(" ^ string_of_expr a ^ "^" ^ string_of_expr b ^ ")"
  | Impl (a, b) -> "(" ^ string_of_expr a ^ "=>" ^ string_of_expr b ^ ")"
  | Bi (a, b) -> "(" ^ string_of_expr a ^ "<=>" ^ string_of_expr b ^ ")"
  | Neg a -> "(" ^ "!" ^ string_of_expr a ^ ")"
