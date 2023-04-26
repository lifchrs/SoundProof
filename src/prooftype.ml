module type BasicFunctions = sig
  type t

  val compare : t -> t -> bool
  val to_string : t -> string
end

module type ProofType = sig
  type t

  include BasicFunctions with type t := t
end

module type ProofStorage = sig
  module T : ProofType

  type expr = T.t

  val curr_goal : expr option ref
  val set_curr_goal : expr option -> unit
  val history : expr list ref
  val add_to_history : expr -> bool -> unit
end

module type ProofMaker = functor (TYPE : ProofType) ->
  ProofStorage with module T = TYPE
