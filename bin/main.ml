open Verifier
open Command
open Proof

let help_options =
  "HELP MENU: \n\
  \ Enter \"help logic\" for help with logic syntax \n\
  \ Enter \"help set\" for help with set syntax \n\
  \ Enter \"help input\" for help with reading in from a file \n\
  \ Enter \"help save\" for help with writing to a file \n\
  \ Common commands: \"quit\" to quit, \"end\" to end current proof, \
   \"history\" to get current proof history"

let help_logic =
  "LOGIC PROOF INFORMATION: \n\
  \ To start a logic proof, enter \"start logic\". \n\
  \ Please enter one of the following keywords: \"SHOW\", \"ASSUME\", \
   \"VERIFY\" followed by an expression. \n\
  \ Expressions can contain conjugation, disjunction, implication, \
   bi-implication, or negation. \n\
  \ These are denoted by \"^\", \"v\", \"=>\", \"<=>\", and \"!\" respectively. \n\
  \ All expressions should contain only capital letters to represent \
   propositions. \n\
  \ Operations are applied to the remainder of an expression !A v B is \
   equivalent to !(A v B). \n\
  \ To negate A by itself in that expression, write (!A) v B.\n\
  \ Please remember to use proper parentheses to impose order of operations."

let help_set =
  "SET PROOF INFORMATION: \n\
  \ To start a set proof, enter \"start set\". \n\
  \ Please enter one of the following keywords: \"SHOW\", \"ASSUME\", \
   \"VERIFY\" followed by an expression. \n\
  \ Expressions can contain intersection, union, difference, or complement. \n\
  \ These are denoted by  \"^\", \"v\", \"\\\\\", and \"Comp\" respectively. \n\
  \ All expressions should contain only capital letters. \n\
  \ Operations are applied to the remainder of an expression Comp A v B is\n\
  \   equivalent to Comp (A v B). \n\
  \ To take the complement of A by itself in that expression, write (Comp A) v \
   B.\n\
  \ Please remember to use proper parentheses to impose order of operations."

let help_input =
  "INPUTTING PROOF INFORMATION: \n\
  \ To input a proof from a file, start the proof with \"start [logic // set] \
   read [name of file]\" \n\
  \ Files should be placed in the [data] directory. Write only the name \
   following \"data\\\". Do not include \".txt\" \n\
  \ The system should automatically read in the steps from the file, and allow \
   you to continue as usual, either from where the file ended, or by starting \
   a new proof."

let help_save =
  "SAVING PROOF INFORMATION: \n\
  \ To save your proof, start the proof with \"start [logic // set] save [name \
   of proof]\" \n\
  \ Proceed as normal, based on which type of proof you entered. As usual, end \
   the proof with \"end\""

(* THE FOLLOWING TWO FUNCTIONS ARE TAKEN FROM ASSIGNMENT A2 OF 3110*)

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

let proof_type = ref ""
let out_file = "data/out.txt"
let oc = open_out_gen [ Open_append; Open_creat ] 0o666 out_file
let to_save = ref false
let write_out str = Printf.fprintf oc "%s\n" str
let command_list = ref []

let rec get_cmds_from_file ic acc =
  try get_cmds_from_file ic (input_line ic :: acc)
  with End_of_file -> List.rev acc

(** Support for starting new proof with IO. Modifies state accordingly to
    properly read in from file or write out to file*)
let set_proof_backend = function
  | "logic" :: "save" :: name ->
      type_out_slowly
        "Beginning logic proof process.\n\
        \    Remember, if at any point you need help,  just type the command \
         \"help\"";
      proof_type := "logic";
      to_save := true;
      write_out ("Name: " ^ String.concat " " name);
      write_out "Type: Logic Proof";
      true
  | "set" :: "save" :: name ->
      type_out_slowly
        "Beginning set proof process.\n\
        \    Remember, if at any point you need help,  just type the command \
         \"help\"";
      proof_type := "set";
      to_save := true;
      write_out ("Name: " ^ String.concat " " name);
      write_out "Type: Set Proof";
      true
  | "logic" :: "read" :: [ filename ] -> (
      proof_type := "logic";
      try
        let ic = open_in ("data/" ^ filename ^ ".txt") in
        command_list := get_cmds_from_file ic [];
        true
      with
      | Sys_error msg ->
          type_out_slowly ("Error: " ^ msg);
          false
      | exn ->
          type_out_slowly ("Error: " ^ Printexc.to_string exn);
          false)
  | "set" :: "read" :: [ filename ] -> (
      proof_type := "set";
      try
        let ic = open_in ("data/" ^ filename ^ ".txt") in
        command_list := get_cmds_from_file ic [];
        true
      with
      | Sys_error msg ->
          type_out_slowly ("Error: " ^ msg);
          false
      | exn ->
          type_out_slowly ("Error: " ^ Printexc.to_string exn);
          false)
  | _ ->
      type_out_slowly
        "Unknown command. Please refer to \"help\" for proper syntax to start \
         a proof";
      false

(** Support for commands "help" and "quit"*)
let one_word_cmds cmd =
  match String.lowercase_ascii cmd with
  | "help" -> type_out_slowly help_options
  | "quit" ->
      print_endline "Exiting now.";
      write_out "";
      close_out oc;
      exit 0
  | _ ->
      type_out_slowly
        "Unknown command. Please refer to \"help\" for proper syntax to start \
         a proof"

(** Support for starting new proof without any IO*)
let simple_new_proof cmd =
  match String.lowercase_ascii cmd with
  | "logic" ->
      type_out_slowly
        "Beginning logic proof process.\n\
        \    Remember, if at any point you need help,  just type the command \
         \"help\"";
      proof_type := "logic";
      to_save := false;
      true
  | "set" ->
      type_out_slowly
        "Beginning set proof process.\n\
        \    Remember, if at any point you need help,  just type the command \
         \"help\"";
      proof_type := "set";
      to_save := false;
      true
  | _ ->
      type_out_slowly
        "Unknown command. Please refer to \"help\" for proper syntax to start \
         a proof";
      false

(** Determines which help message to print out*)
let get_help_type cmd =
  match String.lowercase_ascii cmd with
  | "logic" -> type_out_slowly help_logic
  | "set" -> type_out_slowly help_set
  | "input" -> type_out_slowly help_input
  | "save" -> type_out_slowly help_save
  | _ ->
      type_out_slowly
        "Unknown command. Please refer to \"help\" for proper syntax to start \
         a proof"

(** Starts a new proof based on user commands. Repeatedly loops until a proper
    command is entered, or the user decides to quit*)
let rec start_new_proof () =
  let input =
    String.split_on_char ' '
      (try read_line ()
       with End_of_file ->
         print_endline "Exiting now.";
         close_out oc;
         exit 0)
  in
  match input with
  | [] ->
      type_out_slowly "Please enter a non-empty command";
      start_new_proof ()
  | [ cmd ] ->
      one_word_cmds cmd;
      start_new_proof ()
  | [ "start"; newtype ] ->
      if simple_new_proof newtype then () else start_new_proof ()
  | [ "help"; helptype ] ->
      get_help_type helptype;
      start_new_proof ()
  | "start" :: cmds2 ->
      if set_proof_backend cmds2 then () else start_new_proof ()
  | _ ->
      type_out_slowly
        "Unknown command. Please refer to \"help\" for proper syntax to start \
         a proof";
      start_new_proof ()

(** Prints the history of the current proof to console*)
let get_history () =
  let str =
    match !proof_type with
    | "logic" ->
        "History: "
        ^ pp_list pp_string
            (List.map
               (fun x -> Command.string_of_logic_expr x)
               !LOGIC_PROOF.history)
    | "set" ->
        "History: "
        ^ pp_list pp_string
            (List.map
               (fun x -> Command.string_of_set_expr x)
               !SET_PROOF.history)
    | _ -> "No proof in progress. Please start a proof"
  in
  type_out_slowly str

(** Checks if input is valid, and returns proper command if it is. Loops
    repeatedly until the user enters a proper command or quits. *)
let rec get_command () =
  let str =
    match !command_list with
    | cmd :: rest ->
        command_list := rest;
        type_out_slowly cmd;
        cmd
    | [] -> (
        try read_line ()
        with End_of_file ->
          print_endline "Exiting now.";
          write_out "";
          close_out oc;
          exit 0)
  in
  match String.lowercase_ascii str with
  | "quit" ->
      print_endline "Exiting now.";
      write_out "";
      close_out oc;
      exit 0
  | "help" ->
      type_out_slowly help_options;
      get_command ()
  | "end" ->
      print_endline
        "Proof cleared. Please quit with the command \"quit\" or start a new \
         proof. Type \"help\" for help";
      LOGIC_PROOF.clear_proof ();
      SET_PROOF.clear_proof ();
      if !to_save then (
        write_out "End proof.";
        write_out "");
      proof_type := "";
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
      | [ "history" ] ->
          get_history ();
          get_command ()
      | [ "help"; helptype ] ->
          get_help_type helptype;
          get_command ()
      | h :: t -> (h, t))

(** Loops indefinitely until user decides to quit. It handles different commands
    based on the proof type, such as adding assumptions, setting goals, and
    performing verifications. It raises exceptions for malformed input and
    continues the loop until valid input is provided. *)
let rec proof_loop () =
  let _ =
    try
      let cmd, expr_str = get_command () in
      match !proof_type with
      | "logic" -> (
          let expr = parse_logic expr_str in
          match cmd with
          | "ASSUME" ->
              if LOGIC_PROOF.add_to_history expr true then
                if !to_save then
                  write_out ("ASSUME" ^ " " ^ Command.string_of_logic_expr expr)
          | "SHOW" ->
              if LOGIC_PROOF.set_curr_goal (Some expr) then
                if !to_save then
                  write_out ("SHOW" ^ " " ^ Command.string_of_logic_expr expr)
          | "VERIFY" ->
              if LOGIC_PROOF.add_to_history expr false then
                if !to_save then
                  write_out ("VERIFY" ^ " " ^ Command.string_of_logic_expr expr)
          | _ -> raise Malformed)
      | "set" -> (
          let expr = parse_set expr_str in
          match cmd with
          | "ASSUME" ->
              if SET_PROOF.add_to_history expr true then
                if !to_save then
                  write_out ("ASSUME" ^ " " ^ Command.string_of_set_expr expr)
          | "SHOW" ->
              if SET_PROOF.set_curr_goal (Some expr) then
                if !to_save then
                  write_out ("SHOW" ^ " " ^ Command.string_of_set_expr expr)
          | "VERIFY" ->
              if SET_PROOF.add_to_history expr false then
                if !to_save then
                  write_out ("VERIFY" ^ " " ^ Command.string_of_set_expr expr)
          | _ -> raise Malformed)
      | _ -> failwith "should not happen"
    with Malformed | Empty ->
      type_out_slowly
        "Error, Malformed string detected. Please make sure you enter a \
         keyword followed by an expression.\n\
        \ Type \"help\" at any point if you need help";
      proof_loop ()
  in
  proof_loop ()

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  start_new_proof ();
  proof_loop ()

(* Execute the game engine. *)
let () =
  print_endline "";
  type_out_slowly
    "Welcome to Proof Verifier Supreme! Enter the type of proof you would like \
     to proceed with, or \"help\" for more information. \n\
    \ Remember, at any time, you can exit with the command \"quit\". \n\
    \ Good luck!";
  main ()
