val current_logic_goal : Command.logic_expression option ref
(** The current expression we are trying to show, or None if there was no goal
    entered or the current goal was shown*)

val set_current_logic_goal : Command.logic_expression option -> unit
(** Sets current expression we are trying to show*)

val logic_history : Command.logic_expression list ref
(** Gets history of all previous steps in proof *)

val add_to_logic_history : Command.logic_expression -> bool -> unit
(** Adds expression to proof history*)

val current_set_goal : Command.set_expression option ref
(** The current expression we are trying to show, or None if there was no goal
    entered or the current goal was shown*)

val set_current_set_goal : Command.set_expression option -> unit
(** Sets current expression we are trying to show*)

val set_history : Command.set_expression list ref
(** Gets history of all previous steps in proof *)

val add_to_set_history : Command.set_expression -> bool -> unit
(** Adds expression to proof history*)
