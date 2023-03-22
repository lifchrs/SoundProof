open OUnit2
open Verifier
open Command

let parse_tests =
  [
    ( "conjunction of proposition" >:: fun _ ->
      assert_equal "((A)^(B))" ([ "A^B" ] |> parse |> string_of_expr) );
    ( "disjunction of proposition" >:: fun _ ->
      assert_equal "((A)v(B))" ([ "AvB" ] |> parse |> string_of_expr) );
    ( "negation of proposition" >:: fun _ ->
      assert_equal "(!(A))" ([ "!A" ] |> parse |> string_of_expr) );
    ( "implication of propositions" >:: fun _ ->
      assert_equal "((A)=>(B))" ([ "A=>B" ] |> parse |> string_of_expr) );
    ( "biimplication of propositions" >:: fun _ ->
      assert_equal "((A)<=>(B))" ([ "A<=>B" ] |> parse |> string_of_expr) );
    ( "Raises malformed error" >:: fun _ ->
      assert_raises Malformed (fun () ->
          [ "A<==>B" ] |> parse |> string_of_expr) );
    (* ( "No capitalisation raises malformed error" >:: fun _ -> assert_raises
       Malformed (fun () -> [ "A" ] |> parse |> string_of_expr) ); *)
    ( "Operators applied left to right" >:: fun _ ->
      assert_equal "((A)^((B)v(P)))" ([ "A^BvP" ] |> parse |> string_of_expr) );
  ]

let suite = "test suite for A2" >::: List.flatten [ parse_tests ]
let _ = run_test_tt_main suite
