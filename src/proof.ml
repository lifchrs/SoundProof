let current_goal : Command.expression option ref = ref None
let history : Command.expression list ref = ref []

let compare_all e =
  List.map
    (fun x ->
      if Logic.compare e x then true
      else (
        print_endline
          ("Error! Entered expression is not logically equivalent to:"
         ^ Command.string_of_expr x);
        false))
    !history

let set_current_goal e =
  match e with
  | None -> current_goal := None
  | Some e1 ->
      if
        (* Checks if the string of any expression is identical to the one we are
           trying to show*)
        List.exists
          (fun str -> String.equal str (Command.string_of_expr e1))
          (List.map (fun e2 -> Command.string_of_expr e2) !history)
      then (
        print_endline "Goal already shown! Please enter a new goal";
        current_goal := None)
      else current_goal := e

let compare_with_goal e =
  match !current_goal with
  | None -> ()
  | Some exp ->
      if String.equal (Command.string_of_expr e) (Command.string_of_expr exp)
      then (
        print_endline "Goal shown! Please enter a new goal";
        current_goal := None)

let verify_new_step e =
  if List.for_all (fun x -> x) (compare_all e) then (
    history := !history @ [ e ];
    compare_with_goal e)
  else
    print_endline
      "Entered step not logically equivalent to previous steps. Please enter a \
       new step"

let add_to_history e force =
  if force then (
    compare_with_goal e;
    history := !history @ [ e ])
  else verify_new_step e
