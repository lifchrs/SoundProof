(** Determining logical equality of expressions *)

val compare_logic : Command.logic_expression -> Command.logic_expression -> bool
(** [compare_logic e1 e2] is [true] if and only if [e1] is [false] or [e2] is
    [true] for all possible configurations of truth values for the propositions
    involved in [e1] or [e2] *)

val compare_sets : Command.set_expression -> Command.set_expression -> bool
(** [compare_sets A B] is true if and only if [A] is a subset of [B]*)
