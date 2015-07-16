module type ID = 
sig
  type t
  val fromString : string -> t
  val toString : t -> string
  val equal : t * t -> bool
end

type t = string

let fromString str = str;;
let toString t = t;;
let equal ((t1:string), (t2:string)) = t1 = t2
