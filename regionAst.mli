open Ast
open Id

module RegionVar :
sig
  type t
  val dummy : t
  val toString : t -> string
  val fresh : unit -> t
  val equal : t * t -> bool
  val isDummy : t -> bool
end

module RegionVarSet : Set.S with type elt = RegionVar.t

module RegionConstraint: 
sig
  type t (*= True 
          | Outlives of RegionVar.t * RegionVar.t 
          | Equality of RegionVar.t * RegionVar.t 
          | In of RegionVar.t * RegionVarSet.t
          | NotIn of RegionVar.t * RegionVarSet.t
          | Conj of t list*)
  val truee: t
  val equal : RegionVar.t * RegionVar.t -> t
  val notEqual : RegionVar.t * RegionVar.t -> t
  val outlives : RegionVar.t * RegionVar.t -> t
  val inSet: RegionVar.t * RegionVarSet.t -> t
  val notInSet : RegionVar.t * RegionVarSet.t -> t
  val subEq : RegionVarSet.t * RegionVarSet.t -> t
  val conj : t list -> t
  val disj : t list -> t
  val mapRegionVars : (RegionVar.t -> RegionVar.t) -> t -> t
end

module ConstraintSolve :
sig
  type sol_t = {substFn: RegionVar.t -> RegionVar.t;
                residue: RegionConstraint.t;}

  val normalize : RegionConstraint.t -> sol_t
  val abduce : (RegionConstraint.t * RegionConstraint.t) 
              -> RegionConstraint.t
end

module Type :
sig
  type con_app_t = {tycon: Tycon.t;
                    rAlloc: RegionVar.t;
                    rBar: RegionVar.t list;
                    tyArgs: t list;}
  and region_t = {rho: RegionVar.t;
                   rhoAlloc: RegionVar.t;
                   rootObjTy: t}
  and t = Int | Bool | Unit | Unknown | Any
         | Tyvar of Tyvar.t
         | Object of RegionVar.t
         | ConApp of con_app_t
         | Region of region_t
         | Exists of RegionVar.t * t 
  val mkApp : Tycon.t * RegionVar.t * RegionVar.t list * t list -> t
  val toString : t -> string
  val mapRegionVars : (RegionVar.t -> RegionVar.t) -> t -> t
  val map : (t -> t option) -> t -> t
  val frv : t -> RegionVar.t list
  val frvStar : t list -> RegionVar.t list
  val var : Tyvar.t -> t
end

module Expr : 
sig
  type t
  type method_call_t = {meth: t * MethodName.t;
                        rAlloc: RegionVar.t;
                        rBar: RegionVar.t list;
                        args: t list}
  type node =
      Int of int
    | Bool of bool
    | Var of Var.t 
    | FieldGet of t * Field.t
    | MethodCall of method_call_t
    | New of Type.t * t list
end

module Stmt :
sig
  type unpack_dec_t = {rho: RegionVar.t;
                       ty: Type.t;
                       var: Var.t;
                       unpackExp: Expr.t;}
  type t = 
      VarDec of Type.t * Var.t * Expr.t
    | Assn of Var.t * Expr.t
    | FieldSet of Expr.t * Expr.t
    | Expr of Expr.t
    | Seq of t list
    | LetRegion of RegionVar.t * t
    | Open of Expr.t * t
    | OpenAlloc of Expr.t * t
    | UnpackDec of unpack_dec_t
end

module Method :
sig
  type t
  val make : name:MethodName.t -> 
             rhoAlloc: RegionVar.t ->
             rhoBar: RegionVar.t list ->
             phi: RegionConstraint.t ->
             params:(Var.t * Type.t) list -> 
             body:Stmt.t -> 
             ret_type:Type.t -> t
  val name : t -> MethodName.t
  val rhoAlloc : t -> RegionVar.t
  val rhoBar : t -> RegionVar.t list
  val phi : t -> RegionConstraint.t
  val params : t -> (Var.t * Type.t) list
  val body : t -> Stmt.t
  val ret_type : t -> Type.t
end

module Con : module type of MakeCon(struct 
                                      include Type
                                    end)

module Class : 
sig
  type t
  val make : tycon:Tycon.t ->
             rhoAlloc: RegionVar.t ->
             rhoBar: RegionVar.t list ->
             phi: RegionConstraint.t ->
             tyvars: (Tyvar.t * Type.t) list ->
             super:Type.t ->
             fields:(Field.t * Type.t) list ->
             ctors:Con.t list -> 
             methods:Method.t list -> t
  val tycon : t -> Tycon.t
  val rhoAlloc : t -> RegionVar.t
  val rhoBar : t -> RegionVar.t list
  val phi : t -> RegionConstraint.t
  val tyvars : t -> (Tyvar.t * Type.t) list
  val super : t -> Type.t
  val fields : t -> (Field.t * Type.t) list
  val ctors : t -> Con.t list
  val methods : t -> Method.t list
  val print : t -> unit
end
