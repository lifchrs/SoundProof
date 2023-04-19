open OUnit2
open Verifier
open Command

let parse_logic_tests =
  [
    ( "conjunction of proposition" >:: fun _ ->
      assert_equal "((A)^(B))" ([ "A^B" ] |> parse_logic |> string_of_logic_expr)
    );
    ( "disjunction of proposition" >:: fun _ ->
      assert_equal "((A)v(B))" ([ "AvB" ] |> parse_logic |> string_of_logic_expr)
    );
    ( "negation of proposition" >:: fun _ ->
      assert_equal "(!(A))" ([ "!A" ] |> parse_logic |> string_of_logic_expr) );
    ( "implication of propositions" >:: fun _ ->
      assert_equal "((A)=>(B))"
        ([ "A=>B" ] |> parse_logic |> string_of_logic_expr) );
    ( "biimplication of propositions" >:: fun _ ->
      assert_equal "((A)<=>(B))"
        ([ "A<=>B" ] |> parse_logic |> string_of_logic_expr) );
    ( "logic raises malformed error" >:: fun _ ->
      assert_raises Malformed (fun () ->
          [ "A<==>B" ] |> parse_logic |> string_of_logic_expr) );
    ( "No capitalisation raises malformed error" >:: fun _ ->
      assert_raises Malformed (fun () ->
          [ "a" ] |> parse_logic |> string_of_logic_expr) );
    ( "!A v B test" >:: fun _ ->
      assert_equal "(!((A)v(B)))"
        ([ "!AvB" ] |> parse_logic |> string_of_logic_expr) );
    ( "(A v B) ^ C (V vs v test))" >:: fun _ ->
      assert_equal "(((A)v(B))^(C))"
        ([ "(AvB)^C" ] |> parse_logic |> string_of_logic_expr) );
  ]

let parse_sets_tests =
  [
    ( "intersection of sets" >:: fun _ ->
      assert_equal "((A)^(B))" ([ "A^B" ] |> parse_set |> string_of_set_expr) );
    ( "union of sets" >:: fun _ ->
      assert_equal "((A)v(B))" ([ "AvB" ] |> parse_set |> string_of_set_expr) );
    ( "complement of set" >:: fun _ ->
      assert_equal "(Comp(A))"
        ([ "Comp (A)" ] |> parse_set |> string_of_set_expr) );
    ( "set raises malformed error" >:: fun _ ->
      assert_raises Malformed (fun () ->
          [ "A vv B" ] |> parse_set |> string_of_set_expr) );
    ( "difference of sets" >:: fun _ ->
      assert_equal "((A)\\(B))" ([ "A\\B" ] |> parse_set |> string_of_set_expr)
    );
    ( "(A v B) ^ C (V vs v test))" >:: fun _ ->
      assert_equal "(((A)v(B))^(C))"
        ([ "(AvB)^C" ] |> parse_set |> string_of_set_expr) );
  ]

let suite =
  "test suite for A2" >::: List.flatten [ parse_logic_tests; parse_sets_tests ]

let _ = run_test_tt_main suite
