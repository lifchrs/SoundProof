(** Parsing of user's commands. *)
type expression =
  | Prop of char
  | Dis of expression * expression
  | Conj of expression * expression
  | Impl of expression * expression
  | Bi of expression * expression
  | Neg of expression

exception Empty
(** Raised when an empty command is parsed. *)

exception Malformed
(** Raised when a malformed expression is parsed. *)

val parse : string list -> expression
val string_of_expr : expression -> string
