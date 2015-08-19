module type RTI_STRUCTS = 
sig
  module CT : Map.S with type key = Ast.Tycon.t
end
module Make (S:RTI_STRUCTS) :
sig
  open S
  (*
   * Tycons in the same order they appear in the source text.
   *)
  val doIt : Ast.Tycon.t list -> Ast.Class.t CT.t -> RegionAst.Class.t CT.t
end
