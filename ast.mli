module type ID = 
sig
  type t
  val fromString : string -> t
  val toString : t -> string
  val equal : t * t -> bool
end

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
end

module Type : 
sig
  type t = Int | Bool | Object
         | ConApp of Tycon.t * t list
         | Tyvar of Tyvar.t
         | Unknown 
  val mkApp : Tycon.t * t list  -> t
  val equal : t * t -> bool
end

module Expr : 
sig
  type t
  type node =
    Int of int
  | Bool of bool
  | Var of Var.t
  | FieldGet of t * Field.t
  | MethodCall of t * MethodName.t * t list
  | New of Tycon.t * Type.t list * t list
  val node : t -> node
  val make : node * Type.t -> t
end

module Stmt : 
sig
  type t = 
      VarDec of Type.t * Var.t * Expr.t
    | Assn of Var.t * Expr.t
    | FieldSet of Expr.t * Field.t * Expr.t
    | Expr of Expr.t
    | Seq of t list
    | Seq2 of t * t
  val dec : Type.t * Var.t * Expr.t -> t
  val assn : Var.t * Expr.t -> t
  val expr : Expr.t -> t
  val seq : t list -> t
  val seq2 : t * t -> t
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

module Con :
sig
  type t
  val make : tycon:Tycon.t -> params:(Var.t * Type.t) list ->
               body:Stmt.t -> t
  val tycon : t -> Tycon.t
  val params : t -> (Var.t * Type.t) list
  val body : t -> Stmt.t 
end

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
end
