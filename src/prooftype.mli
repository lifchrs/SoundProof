(** Stores previous steps in the proof*)

module type BasicFunctions = sig
  type t

  val compare : t -> t -> bool
  (** [compare t1 t2] is [true] if [t1] is equal to [t2], or [false] otherwise. *)

  val to_string : t -> string
  (** [to_string v] is a textual representation of [v]. *)
end

(** A module that matches [ProofType] is suitable for use as the type of proof
    in [ProofStorage]. Must be comparable and stringable *)
module type ProofType = sig
  type t

  include BasicFunctions with type t := t
end

(** [ProofStorage] stores the state of the proof *)
module type ProofStorage = sig
  module T : ProofType
  (** [T] is a module representing the type of proof and functions on that type. *)

  type expr = T.t
  (** [expr] is the type of expression in the proof*)

  val curr_goal : expr option ref
  (** The current expression we are trying to show, or None if there was no goal
      entered or the current goal was shown*)

  val set_curr_goal : expr option -> unit
  (** Sets current expression we are trying to show*)

  val history : expr list ref
  (** Gets history of all previous steps in proof *)

  val add_to_history : expr -> bool -> unit
  (** Adds expression to proof history*)
end

(** A [ProofMaker] is a functor that makes [ProofStorage] out of a module
    representing the type of proof. *)
module type ProofMaker = functor (TYPE : ProofType) ->
  ProofStorage with module T = TYPE
