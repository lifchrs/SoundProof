(** Parsing of user's commands. *)

type logic_expression =
  | Prop of char
  | Dis of logic_expression * logic_expression
  | Conj of logic_expression * logic_expression
  | Impl of logic_expression * logic_expression
  | Bi of logic_expression * logic_expression
  | Neg of logic_expression

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
val string_of_logic_expr : logic_expression -> string
val parse_set : string list -> set_expression
val string_of_set_expr : set_expression -> string
