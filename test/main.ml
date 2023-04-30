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
    parse_logic_test " ^ when more is to be read" "(((A)^(B))v((C)^(D)))"
      "(A^B) v (C^D)";
    parse_logic_test " ! when more is to be read" "((!(B))v(!(D)))"
      "(!B) v (!D)";
    parse_logic_error_test "raises malformed with unbalanced parens" Malformed
      [ "())" ];
    parse_logic_test " => when more is to be read" "(((A)=>(B))v((C)=>(D)))"
      "(A=>B) v (C=>D)";
    parse_logic_test " <=> when more is to be read" "(((A)<=>(B))v((C)<=>(D)))"
      "(A<=>B) v (C<=>D)";
    parse_logic_test "negation of chained ands" "(!((A)^((B)^((C)^(B)))))"
      "!(A^B^C^B)";
    parse_logic_test "simple implication" "((A)=>(B))" "A => B";
    parse_logic_test "multiple ors" "((A)v((B)v((C)v(D))))" "A v B v C v D";
    parse_logic_test "implication, and, or" "((A)<=>((V)^((C)v(D))))"
      "A <=> V^(CvD)";
    parse_logic_test "negation of implication" "(!((A)=>((B)v(D))))"
      "!(A => BvD)";
    parse_logic_test "negation of implication with and"
      "(!((A)v(((B)=>(C))^(D))))" "!(A v ((B => C)^D))";
    parse_logic_test "5 chained ors" "((A)v((B)v((C)v((D)v(E)))))"
      "A v B v C v D v E";
    parse_logic_test "negation of implication" "(!((A)=>(A)))" "!(A => A)";
    parse_logic_test "Identity implication" "((A)=>(A))" "A => A";
    parse_logic_test "identity iff" "((A)<=>(A))" "A <=> A";
    parse_logic_test "negation of ors" "(!((A)v(!(B))))" "!(A v !B)";
    parse_logic_test "chained implication" "((A)=>((B)=>(C)))" "A => (B => C)";
    parse_logic_test "biimplication with and" "((A)<=>((B)^(C)))"
      "A <=> (B ^ C)";
    parse_logic_test "commutativity of and" "(((P)^(B))=>((B)^(P)))"
      "(P ^ B) => (B ^P)";
    parse_logic_test "Biimpl with and" "(((U)^(P))=>((U)<=>(P)))"
      "(U^P) => (U <=> P)";
    parse_logic_test "implication with not" "((B)=>(!(P)))" "B => !P";
    parse_logic_test "bimplication implies implication"
      "(((A)<=>(D))=>((D)=>(A)))" "(A <=> D) => (D => A)";
    parse_logic_test "and anded with or (different prop letters too)"
      "(((A)^(D))^((B)v(D)))" "(A ^ D) ^ (B v D)";
    parse_logic_test "negation implies and implication"
      "(!((A)=>((A)v((D)=>(A)))))" "!(A) => (A v D => A)";
    parse_logic_test "(C) different proposition names" "(C)" "C";
    parse_logic_test "(D) different proposition names" "(D)" "D";
    parse_logic_test "negation of chain of ors" "(!((A)v((A)v((A)v(A)))))"
      "!(A v A v A v A)";
    parse_logic_test "chain of ands" "((A)^((A)^((A)^((A)^((A)^(A))))))"
      "A ^ A ^A^ A ^ A^A";
    parse_logic_test "chain of ands with parentheses precedence"
      "(((A)^(A))^(A))" "(A^A) ^ A";
    parse_logic_test "ors of V" "((V)v((V)v(V)))" "V v V v V";
    parse_logic_test "ors and ands of V" "((V)v((V)^(V)))" "V v V ^ V";
    parse_logic_test "negation of biimplication (AB)" "(!((A)<=>(B)))"
      "!(A <=> B)";
    parse_logic_test "implication implies implication"
      "(((A)=>(B))=>((C)=>(D)))" "(A=>B) => (C => D)";
    parse_logic_test "testing different prop names with ands of ors"
      "(((A)v(B))^(((C)v(D))^((E)v(D))))" "(AvB)^((CvD)^(EvD ))";
    parse_logic_test "bimplication with implication" "((A)<=>((B)=>(C)))"
      "A <=> (B => C)";
    parse_logic_test "negation of or and with ands" "(!(((A)v(D))^((C)^(D))))"
      "!(AvD) ^ C^D";
    parse_logic_test "ors of negations" "(!((C)v(!((D)v(!(A))))))"
      "!(C) v !(D) v !A";
    parse_logic_test "neg of and" "(!((A)^(B)))" "!(A^B)";
    parse_logic_test "or of neg" "((!(A))v(B))" "((!A) v (B))";
    parse_logic_test "peels parentheses correctly" "(A)" "((((((((A))))))))";
    parse_logic_test "ors with precedence" "(((A)v(B))v(C))" "(((A) v B) v C)";
    parse_logic_test "A or A" "((A)v(A))" "A  v  A";
    parse_logic_test "spacing doesn't matter" "((B)^((A)v(D)))"
      "B ^   (A v    D)";
    parse_logic_test "multiple negations" "(!(!(!(!(!(!(A)))))))"
      "!(!(!(!(!(!(A))))))";
    parse_logic_test "multiple (fewer) negations" "(!(!(!(A))))" "!(!(!A))";
    parse_logic_test "nested negations " "(!((B)^(!((B)^(!(B))))))"
      "!(B^!(B^!(B)))";
    parse_logic_test "nested implications" "((A)=>((B)=>((C)=>((D)=>(E)))))"
      "A => (B => (C => (D => E)))";
    parse_logic_test "and of not of and" "((A)^(!((B)^(A))))" "A ^ !(B^A)";
    parse_logic_test "biimplication of and + or" "((A)<=>((B)^((C)v(A))))"
      "A <=> B ^ (C v A) ";
    parse_logic_test "implication of biimplication" "((A)=>((B)<=>(C)))"
      "A => (B <=> C)";
    parse_logic_test "implication of negations" "((!(C))=>(!(D)))"
      "(!C) => (!D)";
    parse_logic_test "negation of an implication using A" "(!((A)=>(A)))"
      "!(A => A)";
    parse_logic_error_test "unbalanced parentheses" Malformed [ "()))" ];
    parse_logic_error_test "double A (AA)" Malformed [ "AA" ];
    parse_logic_error_test "unbalanced parentheses with A" Malformed [ "((A)" ];
    parse_logic_error_test "singularly empty parens" Malformed [ "()" ];
    parse_logic_error_test "nested empty parens" Malformed [ "(())" ];
    parse_logic_error_test "parens around lowercase" Malformed [ "(a)" ];
    parse_logic_error_test "AB" Malformed [ "AB" ];
    parse_logic_error_test "double or (vv) " Malformed [ "AvvB" ];
    parse_logic_error_test "double and (^^)" Malformed [ "A ^^ C" ];
    parse_logic_error_test "BB in larger expression" Malformed
      [ "A v (BB ^ D)" ];
    parse_logic_error_test "singular and" Malformed [ "v" ];
    parse_logic_error_test "singular or" Malformed [ "^" ];
    parse_logic_error_test "singular neg" Malformed [ "!" ];
    parse_logic_error_test "singular impl" Malformed [ "=>" ];
    parse_logic_error_test "extra = in impl sign" Malformed [ "A ==> B" ];
    parse_logic_error_test "impl then and" Malformed [ "A => ^B" ];
    parse_logic_error_test "and then impl" Malformed [ "A ^ => B" ];
    parse_logic_error_test "or then impl" Malformed [ "A v => E" ];
    parse_logic_error_test "triple and" Malformed [ "A ^^^ B" ];
    parse_logic_error_test "AB in chain of ands" Malformed [ "A ^ AB ^ D" ];
    parse_logic_error_test "neg at end" Malformed [ "B!" ];
    parse_logic_error_test "trailing or" Malformed [ "Av" ];
    parse_logic_error_test "incomplete inside parens" Malformed [ "(Bv)A^A" ];
    parse_logic_error_test "beginning or" Malformed [ "vA^V" ];
    parse_logic_error_test "incomplete outside parens" Malformed
      [ "C(D^(AvA))" ];
    parse_logic_error_test "Prop Neg Prop" Malformed [ "B!D" ];
    parse_logic_error_test "and then biimpl" Malformed [ "A => (D^<=>D)" ];
    parse_logic_error_test "<= error" Malformed [ "A <= D" ];
    parse_logic_error_test "empty parens next to eachother" Malformed
      [ "()()(A)" ];
    parse_logic_error_test "empty parens with lower case" Malformed
      [ "()()(a)" ];
    parse_logic_error_test "just empty parens" Malformed [ "()()()" ];
    parse_logic_error_test "empty parens before correct expr" Malformed
      [ "(()) A => B" ];
    parse_logic_test "and with neg of or" "((A)v(!((B)v(B))))" "A v !(BvB)";
    parse_logic_test "A impl neg A" "((A)=>(!(A)))" "A => !A";
    parse_logic_test "B impl of neg of chain of and"
      "((A)<=>(!((A)^((B)^(C)))))" "A <=> !((A)^B^C)";
    parse_logic_test "neg of and of impl" "(!((B)^((A)=>(B))))" "!(B^(A=>B))";
    parse_logic_test "A and not A implication" "((A)^((!(A))=>(D)))"
      "A^(!A) => D";
    parse_logic_error_test "equals sign not allowed" Malformed [ "A = B" ];
    parse_logic_test "and of some negs" "((C)^((!(B))^(!(D))))"
      "C ^ (!B) ^ (!D)";
    parse_logic_test "and of negs with precedence" "((D)^((E)v((!(F))^(X))))"
      "D ^ (E v (!F) ^ X)";
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
    parse_set_error_test "unbalanced parentheses" Malformed [ "()))" ];
    parse_set_error_test "double A (AA)" Malformed [ "AA" ];
    parse_set_error_test "unbalanced parentheses with A" Malformed [ "((A)" ];
    parse_set_error_test "singularly empty parens" Malformed [ "()" ];
    parse_set_error_test "nested empty parens" Malformed [ "(())" ];
    parse_set_error_test "parens around lowercase" Malformed [ "(a)" ];
    parse_set_error_test "AB" Malformed [ "AB" ];
    parse_set_error_test "double union (vv) " Malformed [ "AvvB" ];
    parse_set_error_test "double inters (^^)" Malformed [ "A ^^ C" ];
    parse_set_error_test "BB in a larger expression" Malformed
      [ "A v (BB ^ D)" ];
    parse_set_error_test "singular union" Malformed [ "v" ];
    parse_set_error_test "singular inters" Malformed [ "^" ];
    parse_set_error_test "singular diff" Malformed [ "\\" ];
    parse_set_error_test "singular Comp" Malformed [ "Comp" ];
    parse_set_error_test "inters then diff" Malformed [ "A ^ \\ B" ];
    parse_set_error_test "union then diff" Malformed [ "A v \\ E" ];
    parse_set_error_test "triple inters" Malformed [ "A ^^^ B" ];
    parse_set_error_test "AB in chain of inters" Malformed [ "A ^ AB ^ D" ];
    parse_set_error_test "diff at end" Malformed [ "B\\" ];
    parse_set_error_test "trailing inion" Malformed [ "Av" ];
    parse_set_error_test "incomplete inside parens" Malformed [ "(Bv)A^A" ];
    parse_set_error_test "beginning with union" Malformed [ "vA^V" ];
    parse_set_error_test "incomplete outside parens" Malformed [ "C(D^(AvA))" ];
    parse_set_error_test "Set Comp Set" Malformed [ "B Comp D" ];
    parse_set_error_test "inters then diff" Malformed [ "A => (D^\\D)" ];
    parse_set_error_test "empty parens next to eachother" Malformed
      [ "()()(A)" ];
    parse_set_error_test "empty parens with lower case" Malformed [ "()()(a)" ];
    parse_set_error_test "just empty parens" Malformed [ "()()()" ];
    parse_set_error_test "empty parens before correct expr" Malformed
      [ "(()) A v B" ];
    parse_set_test "Comp of chained inters" "(Comp((A)^((B)^((C)^(B)))))"
      "Comp(A^B^C^B)";
    parse_set_test "multiple union" "((A)v((B)v((C)v(D))))" "A v B v C v D";
    parse_set_test "Diff, inters, union" "((A)\\((V)^((C)v(D))))" "A \\ V^(CvD)";
    parse_set_test "Comp of union with inters" "(Comp((A)^((B)v(D))))"
      "Comp(A ^ BvD)";
    parse_set_test "Comp of union with inters" "(Comp((A)v(((B)^(C))^(D))))"
      "Comp(A v ((B ^ C)^D))";
    parse_set_test "5 chained unions" "((A)v((B)v((C)v((D)v(E)))))"
      "A v B v C v D v E";
    parse_set_test "Complement of intersection using A" "(Comp((A)^(A)))"
      "Comp(A ^ A)";
    parse_set_test "Diff with A and A" "((A)\\(A))" "A \\ A";
    parse_set_test "comp of unions" "(Comp((A)v(Comp(B))))" "Comp(A v CompB)";
    parse_set_test "Diff with inters" "((A)\\((B)^(C)))" "A \\ (B ^ C)";
    parse_set_test "commutativity of inters" "(((P)^(B))^((B)^(P)))"
      "(P ^ B) ^ (B ^P)";
    parse_set_test "Diff with unions" "(((U)^(P))^((U)\\(P)))"
      "(U^P) ^ (U \\ P)";
    parse_set_test "Complement of an intersection" "((B)^(Comp(P)))"
      "B ^ Comp P";
    parse_set_test
      "intersection intersected with union (different set letters too)"
      "(((A)^(D))^((B)v(D)))" "(A ^ D) ^ (B v D)";
    parse_set_test "comp inters and unions with precedence"
      "(Comp((A)^((A)v((D)^(A)))))" "Comp(A ^ (A v D ^ A))";
    parse_set_test "(C) different set names" "(C)" "C";
    parse_set_test "(D) different set names" "(D)" "D";
    parse_set_test "complement of the chain of unions"
      "(Comp((A)v((A)v((A)v(A)))))" "Comp(A v A v A v A)";
    parse_set_test "chain of inters" "((A)^((A)^((A)^((A)^((A)^(A))))))"
      "A ^ A ^A^ A ^ A^A";
    parse_set_test "chain of inters with parentheses precedence"
      "(((A)^(A))^(A))" "(A^A) ^ A";
    parse_set_test "unions of V" "((V)v((V)v(V)))" "V v V v V";
    parse_set_test "unions and intersections of V" "((V)v((V)^(V)))"
      "(V) v V ^ V";
    parse_set_test "inters with different precedence" "(((A)^(B))^((C)^(D)))"
      "(A^B) ^ (C ^ D)";
    parse_set_test "testing different set names with inters of unions"
      "(((A)v(B))^(((C)v(D))^((E)v(D))))" "(AvB)^((CvD)^(EvD ))";
    parse_set_test "set difference with intesection" "((A)\\((B)^(C)))"
      "A \\ (B ^ C)";
    parse_set_test "Comp of union and with inters" "(Comp(((A)v(D))^((C)^(D))))"
      "Comp(AvD) ^ C^D";
    parse_set_test "unions of Comp" "(Comp((C)v(Comp((D)v(Comp(A))))))"
      "Comp(C) v Comp(D) v CompA";
    parse_set_test "Comp of inters" "(Comp((A)^(B)))" "Comp(A^B)";
    parse_set_test "union of comp" "((Comp(A))v(B))" "((Comp A) v (B))";
    parse_set_test "peels parentheses correctly (A with multiple parens" "(A)"
      "((((((((A))))))))";
    parse_set_test "unions with precedence using parens" "(((A)v(B))v(C))"
      "(((A) v B) v C)";
    parse_set_test "A union A" "((A)v(A))" "A  v  A";
    parse_set_test "spacing doesn't matter" "((B)^((A)v(D)))" "B ^   (A v    D)";
    parse_set_test "multiple complements"
      "(Comp(Comp(Comp(Comp(Comp(Comp(A)))))))"
      "Comp(Comp(Comp(Comp(Comp(Comp(A))))))";
    parse_set_test "multiple (fewer) comps" "(Comp(Comp(Comp(A))))"
      "Comp(Comp(CompA))";
    parse_set_test "nested comps" "(Comp((B)^(Comp((B)^(Comp(B))))))"
      "Comp(B^Comp(B^Comp(B)))";
    parse_set_test "intersection of Comp of intersection"
      "((A)^(Comp((B)^(A))))" "A ^ Comp(B^A)";
    parse_set_test "Diff of union + inters" "((A)\\((B)^((C)v(A))))"
      "A \\ B ^ (C v A) ";
    parse_set_test "Inters of Diff" "((A)^((B)\\(C)))" "A ^ (B \\ C)";
    parse_set_test "intersection of Comp" "((Comp(C))^(Comp(D)))"
      "(CompC) ^ (CompD)";
    parse_set_test "Comp of inters using A" "(Comp((A)^(A)))" "Comp(A ^ A)";
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
