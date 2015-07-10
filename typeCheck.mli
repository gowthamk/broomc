module type TYPE_CHECK_STRUCTS = 
sig
  module CT : Map.S with type key = Ast.Tycon.t
end
module Make (S:TYPE_CHECK_STRUCTS) :
sig
  open S
  val doIt : Ast.Class.t CT.t -> Ast.Class.t CT.t
end
