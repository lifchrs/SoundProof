(** Storage of previous stages and current task in proof. *)

(** Type of a logic proof, with all necessary functions to perform operations on
    a logic proof *)
module LOGIC_PROOF : sig
  module T : sig
    type t = Command.logic_expression

    val compare : t -> t -> bool
    val to_string : t -> string
    val comparison : t list -> t -> bool
  end

  type expr = Command.logic_expression

  val clear_proof : unit -> unit
  val curr_goal : expr option ref
  val set_curr_goal : expr option -> bool
  val history : expr list ref
  val add_to_history : expr -> bool -> bool
end

(** Type of a set proof, with all necessary functions to perform operations on a
    set proof *)
module SET_PROOF : sig
  module T : sig
    type t = Command.set_expression

    val compare : t -> t -> bool
    val to_string : t -> string
    val comparison : t list -> t -> bool
  end

  type expr = Command.set_expression

  val clear_proof : unit -> unit
  val curr_goal : expr option ref
  val set_curr_goal : expr option -> bool
  val history : expr list ref
  val add_to_history : expr -> bool -> bool
end
