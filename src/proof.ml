let current_goal : Command.logic_expression option ref = ref None
let history : Command.logic_expression list ref = ref []
let compare_all e = List.map (fun x -> Logic.compare e x) !history

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (_ :: _ as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let set_current_goal e =
  match e with
  | None -> current_goal := None
  | Some e1 ->
      if
        (* Checks if the string of any expression is identical to the one we are
           trying to show*)
        List.exists
          (fun str -> String.equal str (Command.string_of_logic_expr e1))
          (List.map (fun e2 -> Command.string_of_logic_expr e2) !history)
      then (
        print_endline "Goal already shown! Please enter a new goal";
        current_goal := None)
      else current_goal := e

let compare_with_goal e =
  match !current_goal with
  | None -> ()
  | Some exp ->
      if
        String.equal
          (Command.string_of_logic_expr e)
          (Command.string_of_logic_expr exp)
      then (
        print_endline "Goal shown! Please enter a new goal";
        current_goal := None)

let verify_new_step e =
  let temp = compare_all e in
  if List.exists (fun x -> x) temp then (
    history := !history @ [ e ];
    compare_with_goal e)
  else
    print_endline "Entered step not logically equivalent to any previous step";
  print_endline
    ("Previous steps: "
    ^ pp_list pp_string
        (List.map (fun x -> Command.string_of_logic_expr x) !history));
  print_endline
    ("Equivalence: "
    ^ pp_list pp_string (List.map (fun x -> if x then "True" else "False") temp)
    )

let add_to_history e force =
  if force then (
    compare_with_goal e;
    history := !history @ [ e ])
  else verify_new_step e
