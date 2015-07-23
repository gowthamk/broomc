open Utils
open Id
module Var : ID
module Tyvar : ID
module Field : ID
module MethodName : ID

module Tycon : 
sig
  type t
  val make : Id.t -> t
  val equal : t*t -> bool
  val toString : t -> string
  val isObject : t -> bool
  val isRegion : t -> bool
end

module Type : 
sig
  type t = Int | Bool | Object | Unit
         | ConApp of Tycon.t * t list
         | Tyvar of Tyvar.t
         | Unknown | Any
  val mkApp : Tycon.t * t list  -> t
  val var : Tyvar.t -> t
  val equal : t * t -> bool
  val mapTyvars : (Tyvar.t -> t) -> t -> t
  val toString : t -> string
  val isRegion : t -> bool
end

module Prim:
sig
  type un_op = Not
  type bin_op = Plus | Minus | Mult | Div
              | GT | LT | And | Or | Equal
  val unOpToString : un_op -> string
  val binOpToString : bin_op -> string
  val typOfBinOp : bin_op -> ((Type.t*Type.t)*Type.t)
end

module Expr : 
sig
  type t
  type node =
  | Null
  | Int of int
  | Bool of bool
  | Var of Var.t
  | FieldGet of t * Field.t
  | MethodCall of t * MethodName.t * t list
  | New of Tycon.t * Type.t list * t list
  | UnOpApp of Prim.un_op * t
  | BinOpApp of t * Prim.bin_op * t
  val null : t
  val node : t -> node
  val typ : t -> Type.t
  val make : node * Type.t -> t
end

module Stmt : 
sig
  type t = 
      VarDec of Type.t * Var.t * Expr.t
    | Assn of Var.t * Expr.t
    | FieldSet of Expr.t * Expr.t
    | Expr of Expr.t
    | Seq of t list
    | LetRegion of t
    | Open of Expr.t * t
    | OpenAlloc of Expr.t * t
  val dec : Type.t * Var.t * Expr.t -> t
  val assn : Var.t * Expr.t -> t
  val expr : Expr.t -> t
  val seq : t list -> t
  val print : t -> unit
end

module Method :
sig
  type t
  val make : name:MethodName.t -> params:(Var.t * Type.t) list 
                -> body:Stmt.t -> ret_type:Type.t -> t
  val name : t -> MethodName.t
  val params : t -> (Var.t * Type.t) list
  val body : t -> Stmt.t
  val ret_type : t -> Type.t
end

module MakeCon : functor(S: sig
                              module Type : STRINGABLE
                              module Stmt: PRINTABLE
                            end) ->
sig
  open S
  type t
  val make : tycon:Tycon.t -> params:(Var.t * Type.t) list ->
               body:Stmt.t -> t
  val tycon : t -> Tycon.t
  val params : t -> (Var.t * Type.t) list
  val body : t -> Stmt.t 
  val print : t -> unit
end

module Con : module type of MakeCon(struct
                                      module Type = Type
                                      module Stmt = Stmt
                                    end)

module Class : 
sig
  type t
  val make : tycon:Tycon.t ->
             tyvars: (Tyvar.t * Type.t) list ->
             super:Type.t ->
             fields:(Field.t * Type.t) list ->
             ctors:Con.t list -> 
             methods:Method.t list -> t
  val tycon : t -> Tycon.t
  val tyvars : t -> (Tyvar.t * Type.t) list
  val super : t -> Type.t
  val fields : t -> (Field.t * Type.t) list
  val ctors : t -> Con.t list
  val methods : t -> Method.t list
  val print : t -> unit
end
