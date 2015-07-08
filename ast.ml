module type ID = 
sig
  type t
  val fromString : string -> t
  val toString : t -> string
  val equal : t * t -> bool
end

let ($) f x = x |> f

module Var = Id
module Tyvar = Id
module Field = Id
module MethodName = Id
module MN = MethodName

module Tycon =
struct
  type t =  Object | T of Id.t

  let make (x : Id.t) : t = match Id.toString x with 
    | "Object" -> Object
    | _ -> T x
  let equal = function 
    | (Object,Object) -> true
    | (T id1, T id2) -> Id.equal (id1,id2)
    | _ -> false
  let toString = function
    | Object -> "Object"
    | (T id) -> Id.toString id
  let isObject = function
    | Object -> true
    | _ -> false
end

module Type =
struct
  type t = Int | Bool | Object
         | ConApp of Tycon.t * t list
         | Tyvar of Tyvar.t
         | Unknown 

  let mkApp (x,y) = ConApp (x,y)
  let rec equal = function
    | (Int,Int) | (Bool,Bool) | (Object,Object) -> true
    | (ConApp (tycon1,typs1), ConApp (tycon2,typs2)) -> 
        Tycon.equal (tycon1,tycon2) && 
        List.for_all2 (fun ty1 ty2 -> equal (ty1,ty2)) typs1 typs2
    | (Tyvar tyv1,Tyvar tyv2) -> Tyvar.equal (tyv1,tyv2)
    | _ -> false
end

module Expr =
struct
  type node =
    Int of int
  | Bool of bool
  | Var of Var.t
  | FieldGet of t * Field.t
  | MethodCall of t * MN.t * t list
  | New of Tycon.t * Type.t list * t list
  and t = T of node * Type.t
  let node (T (n,_)) = n
  let make (n,ty) = T (n,ty)
end

module Stmt = 
struct
  type t = 
      VarDec of Type.t * Var.t * Expr.t
    | Assn of Var.t * Expr.t
    | FieldSet of Expr.t * Field.t * Expr.t
    | Expr of Expr.t
    | Seq of t list
    | Seq2 of t * t
  
  let dec (t,v,e) = VarDec (t,v,e)
  let assn (v,e) = Assn (v,e)
  let expr e = Expr e
  let seq es = Seq es
  let seq2 (e1,e2) = Seq2 (e1,e2)
end

module Method = 
struct
  type t = {
    name     : MN.t;
    params   : (Var.t * Type.t) list;
    body     : Stmt.t;
    ret_type : Type.t;
  }

  let make ~name ~params ~body ~ret_type =
    { name     = name;
      params   = params;
      body     = body;
      ret_type = ret_type; 
    }
  let name m = m.name
  let params m = m.params
  let body m = m.body
  let ret_type m = m.ret_type
end
module Con =
struct
  type t = {
    tycon      : Tycon.t;
    params     : (Var.t * Type.t) list;
    body       : Stmt.t;
  }

  let make ~tycon ~params ~body =
    { tycon      = tycon;
      params     = params;
      body       = body;
     }
  let tycon c = c.tycon
  let params c = c.params
  let body c = c.body
end
module Class =
struct
  type t = {
    tycon   : Tycon.t;
    tyvars  : (Tyvar.t * Type.t) list;
    super   : Type.t;
    fields  : (Field.t * Type.t) list;
    ctors   : Con.t list;
    methods : Method.t list;
  }

  let make ~tycon ~tyvars ~super ~fields ~ctors ~methods
    = { tycon = tycon;
        tyvars = tyvars;
        super   = super;
        fields  = fields;
        ctors   = ctors;
        methods = methods }

  let tycon k = k.tycon
  let tyvars k = k.tyvars
  let super k = k.super
  let fields k = k.fields
  let ctors k = k.ctors
  let methods k = k.methods
end
