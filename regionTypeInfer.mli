module type RTI_STRUCTS = 
sig
  module CT : Map.S with type key = Ast.Tycon.t
end
module Make (S:RTI_STRUCTS) :
sig
  open S
  val doIt : Ast.Class.t CT.t -> RegionAst.Class.t CT.t
end
