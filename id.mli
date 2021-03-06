module type ID = 
sig
  type t
  val fromString : string -> t
  val toString : t -> string
  val equal : t * t -> bool
end

include ID
