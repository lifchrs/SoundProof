open OUnit2
open Verifier
open Command
open Logic

(* Testing for Command compilation unit *)

(** [parse_logic_test name expected_output input] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with
    [\[input\] |> parse_logic |> string_of_logic_expr]. *)
let parse_logic_test (name : string) (expected_output : string) (e1 : string) :
    test =
  name >:: fun _ ->
  assert_equal expected_output
    ([ e1 ] |> parse_logic |> string_of_logic_expr)
    ~printer:(fun x -> x)

(** [parse_logic_error_test name expected_output input] constructs an OUnit test
    named [name] that asserts that [expected_output] is raised when is run
    [input parse_logic |> string_of_logic_expr]. *)
let parse_logic_error_test (name : string) (expected_output : exn)
    (e1 : string list) : test =
  name >:: fun _ ->
  assert_raises expected_output (fun () ->
      e1 |> parse_logic |> string_of_logic_expr)

let parse_logic_tests =
  [
    parse_logic_test "conjunction of proposition" "((A)^(B))" "A^B";
    parse_logic_test "disjunction of proposition" "((A)v(B))" "AvB";
    parse_logic_test "negation of proposition" "(!(A))" "!A";
    parse_logic_test "implication of propositions" "((A)=>(B))" "A=>B";
    parse_logic_test "biimplication of propositions" "((A)<=>(B))" "A<=>B";
    parse_logic_error_test "logic raises malformed error" Malformed [ "A<==>B" ];
    parse_logic_error_test "No capitalisation raises malformed error" Malformed
      [ "a" ];
    parse_logic_error_test "Empty string list raises empty" Empty [];
    parse_logic_test "!A v B test" "(!((A)v(B)))" "!AvB";
    parse_logic_test "(A v B) ^ C (V vs v test))" "(((A)v(B))^(C))" "(AvB)^C";
    parse_logic_test "left parens peeling" "(A)" "((A))";
    parse_logic_error_test "raises malformed with incorrect expression"
      Malformed [ "(()" ];
    parse_logic_test "testing ^ when more is to be read" "(((A)^(B))v((C)^(D)))"
      "(A^B) v (C^D)";
    parse_logic_test "testing ! when more is to be read" "((!(B))v(!(D)))"
      "(!B) v (!D)";
    parse_logic_error_test "raises malformed with unbalanced parens" Malformed
      [ "())" ];
    parse_logic_test "testing => when more is to be read"
      "(((A)=>(B))v((C)=>(D)))" "(A=>B) v (C=>D)";
    parse_logic_test "testing <=> when more is to be read"
      "(((A)<=>(B))v((C)<=>(D)))" "(A<=>B) v (C<=>D)";
  ]

(** [parse_set_test name expected_output input] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with
    [\[input\] |> parse_set |> string_of_set_expr]. *)
let parse_set_test (name : string) (expected_output : string) (e1 : string) :
    test =
  name >:: fun _ ->
  assert_equal expected_output
    ([ e1 ] |> parse_set |> string_of_set_expr)
    ~printer:(fun x -> x)

(** [parse_logic_error_test name expected_output input] constructs an OUnit test
    named [name] that asserts that [expected_output] is raised when is run
    [input parse_logic |> string_of_logic_expr]. *)
let parse_set_error_test (name : string) (expected_output : exn)
    (e1 : string list) : test =
  name >:: fun _ ->
  assert_raises expected_output (fun () ->
      e1 |> parse_set |> string_of_set_expr)

let parse_sets_tests =
  [
    parse_set_test "intersection of sets" "((A)^(B))" "A^B";
    parse_set_test "union of sets" "((A)v(B))" "AvB";
    parse_set_test "complement of set" "(Comp(A))" "Comp (A)";
    parse_set_error_test "improper syntax (vv) raises malformed error" Malformed
      [ "A vv B" ];
    parse_set_test "difference of sets" "((A)\\(B))" "A\\B";
    parse_set_test "(A v B) ^ C (V vs v test))" "(((A)v(B))^(C))" "(AvB)^C";
    parse_set_error_test "Empty string list raises empty" Empty [];
    parse_set_test "testing ^ when more is to be read" "(((A)^(B))v((C)^(D)))"
      "(A^B) v (C^D)";
    parse_set_test "testing Comp when more is to be read"
      "((Comp(B))v(Comp(D)))" "(Comp(B)) v (Comp(D))";
    parse_set_test "testing \\ when more is to be read"
      "(((A)\\(B))v((C)\\(D)))" "(A\\B) v (C\\D)";
    parse_set_error_test "raises malformed with unbalanced parens" Malformed
      [ "())" ];
    parse_set_test "complement of lone set" "(Comp(A))" "Comp (A)";
    parse_set_error_test "Set must be capitalised" Malformed [ "a" ];
    parse_set_error_test "raises malformed with incorrect expression" Malformed
      [ "(()" ];
    parse_set_error_test "raises malformed with incorrect expression AB"
      Malformed [ "AB" ];
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
    compare_logic_test "A ^ not(B) implies not(A <=> B)"
      (Conj (Prop 'A', Neg (Prop 'B')))
      (Neg (Bi (Prop 'A', Prop 'B')))
      true;
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
  "test suite for project"
  >::: List.flatten
         [
           parse_logic_tests;
           parse_sets_tests;
           compare_logic_tests;
           compare_sets_tests;
         ]

let _ = run_test_tt_main suite
