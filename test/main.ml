open OUnit2
open Verifier
open Command
open Logic

(* Testing for Command compilation unit *)

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

(* Testing for Logic compilation unit *)

(** [compare_logic_test name input expected_output] constructs an OUnit test
    named [name] that asserts the quality of [expected_output] with
    [compare_logic input]. *)
let compare_logic_test (name : string) (e1 : logic_expression)
    (e2 : logic_expression) (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (compare_logic e1 e2) ~printer:Bool.to_string

(** [compare_sets_test name input expected_output] constructs an OUnit test
    named [name] that asserts the quality of [expected_output] with
    [compare_sets input]. *)
let compare_sets_test (name : string) (e1 : set_expression)
    (e2 : set_expression) (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (compare_sets e1 e2) ~printer:Bool.to_string

let compare_logic_tests =
  [
    compare_logic_test "A implies A v B" (Prop 'A')
      (Dis (Prop 'A', Prop 'B'))
      true;
    compare_logic_test "A ^ B implies A v B"
      (Conj (Prop 'A', Prop 'B'))
      (Dis (Prop 'A', Prop 'B'))
      true;
    compare_logic_test "B does not imply A ^ B" (Prop 'B')
      (Conj (Prop 'A', Prop 'B'))
      false;
    compare_logic_test "A => B implies not(A) v B"
      (Impl (Prop 'A', Prop 'B'))
      (Dis (Neg (Prop 'A'), Prop 'B'))
      true;
    compare_logic_test "A implies B => A" (Prop 'A')
      (Impl (Prop 'B', Prop 'A'))
      true;
    compare_logic_test "(A v B) ^ (C v D) implies not(A) => (B ^ C) v (B ^ D)"
      (Conj (Dis (Prop 'A', Prop 'B'), Dis (Prop 'C', Prop 'D')))
      (Impl
         ( Neg (Prop 'A'),
           Dis (Conj (Prop 'B', Prop 'C'), Conj (Prop 'B', Prop 'D')) ))
      true;
    compare_logic_test "A ^ B ^ C ^ D implies D v E v F v G"
      (Conj (Prop 'A', Conj (Prop 'B', Conj (Prop 'C', Prop 'D'))))
      (Dis (Prop 'D', Dis (Prop 'E', Dis (Prop 'F', Prop 'G'))))
      true;
    compare_logic_test "(A => B) ^ (B => A) does not imply A v B"
      (Conj (Impl (Prop 'A', Prop 'B'), Impl (Prop 'B', Prop 'A')))
      (Dis (Prop 'A', Prop 'B'))
      false;
  ]

let compare_sets_tests =
  [
    compare_sets_test "A is a subset of A v B" (Set 'A')
      (Union (Set 'A', Set 'B'))
      true;
    compare_sets_test "A ^ B is a subset of A v B"
      (Intersection (Set 'A', Set 'B'))
      (Union (Set 'A', Set 'B'))
      true;
    compare_sets_test "A \\ B is not a subset of A ^ B"
      (Difference (Set 'A', Set 'B'))
      (Intersection (Set 'A', Set 'B'))
      false;
    compare_sets_test "(A ^ C) \\ B is a subset of (A \\ B) ^ (C \\ B)"
      (Difference (Intersection (Set 'A', Set 'C'), Set 'B'))
      (Intersection
         (Difference (Set 'A', Set 'B'), Difference (Set 'C', Set 'B')))
      true;
    compare_sets_test "(A \\ B) ^ (C \\ B) is a subset of (A ^ C) \\ B"
      (Intersection
         (Difference (Set 'A', Set 'B'), Difference (Set 'C', Set 'B')))
      (Difference (Intersection (Set 'A', Set 'C'), Set 'B'))
      true;
    compare_sets_test "Comp (A v B) is a subset of Comp(A) ^ Comp (B)"
      (Comp (Union (Set 'A', Set 'B')))
      (Intersection (Comp (Set 'A'), Comp (Set 'B')))
      true;
    compare_sets_test "Comp (A ^ B) is not a subset of Comp(A) ^ Comp (B)"
      (Comp (Intersection (Set 'A', Set 'B')))
      (Intersection (Comp (Set 'A'), Comp (Set 'B')))
      false;
    compare_sets_test "A is not a subset of B" (Set 'A') (Set 'B') false;
    compare_sets_test
      "Comp((A ^ B) \\ C) is a subset of Comp(A \\ C) v Comp(B \\ C)"
      (Comp (Difference (Intersection (Set 'A', Set 'B'), Set 'C')))
      (Union
         ( Comp (Difference (Set 'A', Set 'C')),
           Comp (Difference (Set 'B', Set 'C')) ))
      true;
  ]

let suite =
  "test suite for A2"
  >::: List.flatten
         [
           parse_logic_tests;
           parse_sets_tests;
           compare_logic_tests;
           compare_sets_tests;
         ]

let _ = run_test_tt_main suite
