let help_message =
  "Thank you for using Proof Verifier supreme. \n"
  ^ "To input your proof, you will enter the statement you want to show, your \
     given assumptions, and the steps between. \n"
  ^ "The supported syntax is as follows: \n"
  ^ "    -show E: Goal is to prove E true \n"
  ^ "    -assume P: Set P as true \n"
  ^ "    -step R: Use R as next step of proof \n"
  ^ "Any of E, P, R, must only contain conjugation, disjunction, implication, \
     bi-implication, or negation.\n"
  ^ "These are denoted by \"^\", \"v\", \"=>\", \"<=>\", \"~\" respectively\n"
  ^ "Finally, use proper parentheses to certify order of operations. For \
     example, \n"
  ^ "While \"(P => Q) ^ ~R\" is proper syntax, \"P => Q ^ ~R\" is not. \n"
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
      print_endline "~~VERIFY FUNCTIONALITY GOES HERE~~";
      main ()
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
