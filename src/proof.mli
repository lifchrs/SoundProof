(** val current_goal : Command.expression option ref (** The current expression
    we are trying to show, or None if there was no goal entered or the current
    goal was shown*)

    val set_current_goal : Command.expression option -> unit (** Sets current
    expression we are trying to show*)

    val history : Command.expression list ref (** Gets history of all previous
    steps in proof *)

    val add_to_history : Command.expression -> bool -> unit (** Adds expression
    to proof history*) *)
