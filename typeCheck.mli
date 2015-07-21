module type TYPE_CHECK_STRUCTS = 
sig
  module CT : Map.S with type key = Ast.Tycon.t
end
module Make (S:TYPE_CHECK_STRUCTS) :
sig
  open S
  open Envs
  val isSubtype : Ast.Class.t CT.t -> Ast.Type.t TyVE.t 
                    -> (Ast.Type.t * Ast.Type.t) -> bool
  val doIt : Ast.Class.t CT.t -> Ast.Class.t CT.t
end
