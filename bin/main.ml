open Verifier
open Command
open Logic

let help_message =
  "Thank you for using Proof Verifier supreme. \n"
  ^ "To input your proof, you will enter the statement you want to show, your \
     given assumptions, and the steps between. \n"
  ^ "The supported syntax is as follows: \n"
  ^ "    -Show E: Goal is to prove E true \n"
  ^ "    -Assume P: Set P as true \n"
  ^ "    -Verify R: Use R as next step of proof \n"
  ^ "Any of E, P, R, must only contain conjugation, disjunction, implication, \
     bi-implication, or negation.\n"
  ^ "These are denoted by \"^\", \"v\", \"=>\", \"<=>\", \"~\" respectively\n"
  ^ "Finally, use proper parentheses to certify order of operations. For \
     example, \n" ^ "\"(P=>Q)^!R\" and \"P=>(Q^!R)\" are both proper syntax. \n"
  ^ "Good luck and happy proving!"

(** Prints provided string by character instead of at once *)
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

(** Checks if the previous input and current input have same truth values*)
let rec proof_loop prev =
  let str =
    try read_line ()
    with End_of_file ->
      print_endline "Exiting now.";
      exit 0
  in
  if str = "quit" then exit 0
  else
    let new_input =
      try
        match parse str with
        | Assume e -> e
        | Show e -> e
        | Verify e -> e
      with
      | Malformed ->
          print_endline
            "Malformed. Please make sure you enter in the correct format";
          proof_loop prev
      | Empty ->
          print_endline
            "Empty. Please make sure you enter in the correct format";
          proof_loop prev
    in
    if compare prev new_input then (
      print_endline "Step is logically sound!";
      proof_loop new_input)
    else (
      print_endline "Incorrect Step. Terminating proof.";
      exit 0)

(** Gets first input from user *)
let rec start_proof _ =
  let str =
    try read_line ()
    with End_of_file ->
      print_endline "Exiting now.";
      exit 0
  in
  if str = "quit" then exit 0
  else
    try
      let prev =
        match parse str with
        | Assume e -> e
        | Show e -> e
        | Verify e -> e
      in
      proof_loop prev
    with
    | Malformed ->
        print_endline
          "Malformed. Please make sure you enter in the correct format";
        start_proof ()
    | Empty ->
        print_endline "Empty. Please make sure you enter in the correct format";
        start_proof ()

(** [main ()] prompts for the game to play, then starts it. *)
let rec main () =
  match read_line () with
  | exception End_of_file -> ()
  | "help" | "Help" | "HELP" ->
      type_out_slowly help_message;
      main ()
  | "start proof"
  | "Start proof"
  | "Start Proof"
  | "START PROOF"
  | "start"
  | "Start"
  | "START" ->
      print_endline "Beginning proof process.";
      let _ = start_proof () in
      ()
  | "quit" | "Quit" | "QUIT" ->
      print_endline "Exiting now.";
      exit 0
  | _ ->
      print_endline
        "Unknown command. If you want to start your proof, type \"Verify\", or \
         \"help\" for more information";
      main ()

(* Execute the game engine. *)
let () =
  print_endline "";
  print_endline
    "Please use the command \"start\" to begin your proof, or \"help\" for \
     more information";
  main ()
