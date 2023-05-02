open Verifier
open Command
open Proof

let proof_type = ref ""

(* type element_option = | SetProof of Command.set_expression list | LogicProof
   of Command.logic_expression list

   type store_proof = (string * element_option) list

   let prev_proofs : store_proof ref = ref [] let to_string (lst : store_proof)
   : unit = match lst with | [] -> print_endline "" | h :: t -> *)

let help_message =
  "Thank you for using Proof Verifier supreme. \n"
  ^ "To input your proof, you will enter the statement you want to show, your \
     given assumptions, and the steps between. \n"
  ^ "The supported syntax is as follows: \n"
  ^ " -Show E: Goal is to prove E true \n" ^ " -Assume P: Set P as true \n"
  ^ " -Verify R: Use R as next step of proof \n"
  ^ "Currently, we support logic and set proofs. \n"
  ^ "In a logic proof, any of E, P, R, must only contain conjugation, \
     disjunction, implication, bi-implication, or negation.\n"
  ^ "These are denoted by \"^\", \"v\", \"=>\", \"<=>\", \"!\" respectively\n"
  ^ "In a set proof, any of E, P, R, must only contain intersection, union, \
     difference, or complement. \n"
  ^ "These are denoted by \"^\", \"v\", \"\\\\\", \"Comp\" respectively \n"
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

let rec start_new_proof () =
  match
    String.lowercase_ascii
      (try read_line ()
       with End_of_file ->
         print_endline "Exiting now.";
         exit 0)
  with
  | "help" ->
      type_out_slowly help_message;
      start_new_proof ()
  | "start logic" ->
      type_out_slowly
        "Beginning logic proof process.\n\
        \    Remember, if at any point you need help,  just type the command \
         \"help\"";
      proof_type := "logic"
  | "start set" ->
      type_out_slowly
        "Beginning set proof process.\n\
        \    Remember, if at any point you need help,  just type the command \
         \"help\"";
      proof_type := "set"
  | "quit" ->
      print_endline "Exiting now.";
      exit 0
  | _ ->
      type_out_slowly
        "Unknown command. If you want to start your proof, type \"START \
         LOGIC\" or \"START SET\", or  \"help\" for more information";
      start_new_proof ()

(** Checks if input is valid, and returns proper command if it is. Otherwise,
    asks the user to re-input a command *)
let rec get_command () =
  let str =
    try read_line ()
    with End_of_file ->
      print_endline "Exiting now.";
      exit 0
  in
  match String.lowercase_ascii str with
  | "quit" ->
      print_endline "Exiting now.";
      exit 0
  | "help" ->
      type_out_slowly help_message;
      get_command ()
  | "end" ->
      print_endline
        "Proof cleared. Please quit with the command \"quit\" or start a new \
         proof with the commands \"START LOGIC\" or \"START SET\"";
      LOGIC_PROOF.clear_proof ();
      SET_PROOF.clear_proof ();
      start_new_proof ();
      get_command ()
  | _ -> (
      let lst_with_empty = String.split_on_char ' ' str in
      let lst =
        List.fold_right
          (fun x acc -> if x <> "" then x :: acc else acc)
          lst_with_empty []
      in
      match lst with
      | [] -> raise Empty
      | h :: t -> (h, t))

(** Repeatedly asks the user for an input and checks if it is equivalent to the
    previous input. Currently only terminates upon entering "quit" or an invalid
    step*)
let rec proof_loop () =
  let _ =
    try
      let cmd, expr_str = get_command () in
      match !proof_type with
      | "logic" -> (
          let expr = parse_logic expr_str in
          match cmd with
          | "Assume" -> LOGIC_PROOF.add_to_history expr true
          | "Show" -> LOGIC_PROOF.set_curr_goal (Some expr)
          | "Verify" -> LOGIC_PROOF.add_to_history expr false
          | _ -> raise Malformed)
      | "set" -> (
          let expr = parse_set expr_str in
          match cmd with
          | "Assume" -> SET_PROOF.add_to_history expr true
          | "Show" -> SET_PROOF.set_curr_goal (Some expr)
          | "Verify" -> SET_PROOF.add_to_history expr false
          | _ -> raise Malformed)
      | _ -> failwith "should not happen"
    with
    | Malformed ->
        type_out_slowly
          "Error, Malformed string\n\
          \    detected. Please make sure you enter a  keyword followed by an \
           expression.\n\
          \    Type \"help\" for help or  \"quit\" to quit";
        proof_loop ()
    | Empty ->
        type_out_slowly
          "Error, Empty string detected. Please make sure you enter a\n\
          \    keyword  followed by an expression. Type \"help\" for help or \
           \"quit\" to quit";
        proof_loop ()
  in
  proof_loop ()

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  start_new_proof ();
  proof_loop

(* Execute the game engine. *)
let () =
  print_endline "";
  type_out_slowly
    "Please use the command \"START LOGIC\" or \"START SET\" to begin your \
     proof, or \"help\" for more information. To end your proof, enter \"END\"";
  main () ()
