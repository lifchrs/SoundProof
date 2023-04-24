(** Determining logical equality of expressions *)

val compare_logic : Command.logic_expression -> Command.logic_expression -> bool
(** Determine if two expressions are equal. Two expressions are equal if they
    have the same truth values for any possible configurations of the truth
    values of their propositions. *)

val compare_sets : Command.set_expression -> Command.set_expression -> bool
(** [compare_sets A B] is true if and only if [A] is a subset of [B]*)
