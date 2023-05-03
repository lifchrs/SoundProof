(**Parsing of user's commands. *)

(**A type representing logic expressions. *)
type logic_expression =
  | Prop of char
  | Dis of logic_expression * logic_expression
  | Conj of logic_expression * logic_expression
  | Impl of logic_expression * logic_expression
  | Bi of logic_expression * logic_expression
  | Neg of logic_expression

(** A type representing set expressions. *)
type set_expression =
  | Set of char
  | Comp of set_expression
  | Intersection of set_expression * set_expression
  | Union of set_expression * set_expression
  | Difference of set_expression * set_expression

exception Empty
(** Raised when an empty command is parsed. *)

exception Malformed
(** Raised when a malformed expression is parsed. *)

val parse_logic : string list -> logic_expression
(** [parse_logic] generates a value of type [logic_expression] corresponding to
    the logic expression command represented by the string list entered*)

val string_of_logic_expr : logic_expression -> string
(** [string_of_logic_expr] creates a string representation of a logical
    expression using the same notation required for user input*)

val parse_set : string list -> set_expression
(** [parse_set] generates a value of type [set_expression] corresponding to the
    set expression command represented by the string list entered*)

val string_of_set_expr : set_expression -> string
(** [string_of_set_expr] creates a string representation of a set expression
    using the same notation required for user input*)
