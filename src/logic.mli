(** (** Determining logical equality of expressions *)

    val compare : Command.expression -> Command.expression -> bool (** Determine
    if two expressions are equal. Two expressions are equal if they have the
    same truth values for any possible configurations of the truth values of
    their propositions. *) *)
