(** Parsing of user's commands. *)

type binary_op = Conj | Dis | Impl | Bi
type unary_op = Neg

type expression =
  | True
  | False
  | Prop of char
  | Dis of expression * expression
  | Conj of expression * expression
  | Impl of expression * expression
  | Bi of expression * expression
  | Neg of expression

type command =
  | Assume of expression
  | Show of expression
  | Verify of expression

exception Empty
(** Raised when an empty command is parsed. *)

exception Malformed
(** Raised when a malformed expression is parsed. *)

val parse : string -> command
