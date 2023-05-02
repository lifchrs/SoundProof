open Prooftype

module Make : ProofMaker =
functor
  (TYPE : ProofType)
  ->
  struct
    module T = TYPE

    type expr = T.t

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

    let curr_goal : expr option ref = ref None
    let history : expr list ref = ref []

    let clear_proof () =
      curr_goal := None;
      history := []

    let set_curr_goal e : bool =
      match e with
      | None ->
          curr_goal := None;
          false
      | Some e1 ->
          if
            (* Checks if the string of any expression is identical to the one we
               are trying to show*)
            List.exists
              (fun str -> String.equal str (T.to_string e1))
              (List.map (fun e2 -> T.to_string e2) !history)
          then (
            print_endline "Goal already shown! Please enter a new goal";
            curr_goal := None;
            false)
          else (
            curr_goal := e;
            true)

    let compare_with_goal e =
      match !curr_goal with
      | None -> ()
      | Some exp ->
          if String.equal (T.to_string e) (T.to_string exp) then (
            print_endline "Goal shown! Please enter a new goal";
            curr_goal := None)

    (** Verifies if an expression is sound based on previous expressions. If [b]
        is [true] then verifies among all previous steps, and if [b] is false
        then verifies among previous step*)
    let verify_new_step (e : expr) : bool =
      if T.comparison !history e then (
        history := !history @ [ e ];
        compare_with_goal e;
        true)
      else (
        print_endline
          "Entered step not logically equivalent with comparison method";
        print_endline
          ("Previous steps: "
          ^ pp_list pp_string (List.map (fun x -> T.to_string x) !history));
        false)

    let add_to_history (e : expr) force : bool =
      if force then (
        compare_with_goal e;
        history := !history @ [ e ];
        true)
      else verify_new_step e
  end

module LOGIC = struct
  type t = Command.logic_expression

  let compare x y = Logic.compare_logic x y
  let to_string x = Command.string_of_logic_expr x

  let comparison lst e =
    match lst with
    | [] -> false
    | acc :: t ->
        compare (List.fold_left (fun x acc -> Command.Conj (x, acc)) acc t) e
end

module LOGIC_PROOF = Make (LOGIC)

module SET = struct
  type t = Command.set_expression

  let compare x y = Logic.compare_sets x y
  let to_string x = Command.string_of_set_expr x

  let comparison lst e =
    match lst with
    | [] -> false
    | _ -> compare (List.hd (List.rev lst)) e
end

module SET_PROOF = Make (SET)
