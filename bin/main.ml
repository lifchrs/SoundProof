open Verifier
open Command

type command =
  | Assume of logic_expression
  | Show of logic_expression
  | Verify of logic_expression

let help_message =
  "Thank you for using Proof Verifier supreme. \n"
  ^ "To input your proof, you will enter the statement you want to show, your \
     given assumptions, and the steps between. \n"
  ^ "The supported syntax is as follows: \n"
  ^ " -Show E: Goal is to prove E true \n" ^ " -Assume P: Set P as true \n"
  ^ " -Verify R: Use R as next step of proof \n"
  ^ "Any of E, P, R, must only contain conjugation, disjunction, implication, \
     bi-implication, or negation.\n"
  ^ "These are denoted by \"^\", \"v\", \"=>\", \"<=>\", \"!\" respectively\n"
  ^ "Finally, use proper parentheses to certify order of operations. For  \
     example, \n"
  ^ "\"(P=>Q)^!R\" and \"P=>(Q^!R)\" are both proper syntax with different  \
     meanings. \n" ^ "Good luck and happy proving!"

(** Causes string to appear like it is printing by letter instead of appearing *)
let type_out_slowly str =
  let len = String.length str in
  let rec loop i =
    if i < len then (
      print_char str.[i];
      flush stdout;
      (*Adjust value of Unix.select to change typing speed*)
      ignore (Unix.select [] [] [] 0.01);
      loop (i + 1))
  in
  loop 0;
  print_newline ()

(** Checks if input is valid, and returns proper command if it is. Otherwise,
    asks the user to re-input a command *)
let rec get_command _ =
  let str =
    try read_line ()
    with End_of_file ->
      print_endline "Exiting now.";
      exit 0
  in
  if String.lowercase_ascii str = "quit" then (
    print_endline "Exiting now.";
    exit 0)
  else if String.lowercase_ascii str = "help" then (
    type_out_slowly help_message;
    get_command ())
  else
    let lst_with_empty = String.split_on_char ' ' str in
    let lst =
      List.fold_right
        (fun x acc -> if x <> "" then x :: acc else acc)
        lst_with_empty []
    in
    try
      match lst with
      | [] -> raise Empty
      | h :: t ->
          if h = "Assume" then Assume (parse_logic t)
          else if h = "Show" then Show (parse_logic t)
          else if h = "Verify" then Verify (parse_logic t)
          else raise Malformed
    with
    | Malformed ->
        type_out_slowly
          "Error, Malformed string\n\
          \    detected. Please make sure you enter a  keyword followed by an \
           expression.\n\
          \    Type \"help\" for help or  \"quit\" to quit";
        get_command ()
    | Empty ->
        type_out_slowly
          "Error, Empty string detected. Please make sure you enter a\n\
          \    keyword  followed by an expression. Type \"help\" for help or \
           \"quit\" to quit";
        get_command ()

(** Repeatedly asks the user for an input and checks if it is equivalent to the
    previous input. Currently only terminates upon entering "quit" or an invalid
    step*)
let rec proof_loop _ =
  match get_command () with
  | Assume e ->
      Proof.add_to_history e true;
      proof_loop ()
  | Show e ->
      Proof.set_current_goal (Some e);
      proof_loop ()
  | Verify e ->
      Proof.add_to_history e false;
      proof_loop ()

(** [main ()] prompts for the game to play, then starts it. *)
let rec main () =
  match
    String.lowercase_ascii
      (try read_line ()
       with End_of_file ->
         print_endline "Exiting now.";
         exit 0)
  with
  | "help" ->
      type_out_slowly help_message;
      main ()
  | "start" ->
      type_out_slowly
        "Beginning proof process.\n\
        \    Remember, if at any point you need help,  just type the command \
         \"help\"";
      let _ = proof_loop () in
      ()
  | "quit" ->
      print_endline "Exiting now.";
      exit 0
  | _ ->
      type_out_slowly
        "Unknown command. If you want to start your proof,\n\
        \    type \"start\", or  \"help\" for more information";
      main ()

(* Execute the game engine. *)
let () =
  print_endline "";
  type_out_slowly
    "Please use the command \"start\" to begin your proof, or \"help\" for \
     more information";
  main ()
