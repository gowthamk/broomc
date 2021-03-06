module Var = Id
module Tyvar = Id
module Field = Id
module MethodName = Id
module MN = MethodName
open Format
open Utils

module Tycon =
struct
  type t =  Object | Region | T of Id.t

  let make (x : Id.t) : t = match Id.toString x with 
    | "Object" -> Object
    | "Region" -> Region
    | _ -> T x
  let equal = function 
    | (Object,Object) -> true
    | (Region,Region) -> true
    | (T id1, T id2) -> Id.equal (id1,id2)
    | _ -> false
  let toString = function
    | Object -> "Object"
    | Region -> "Region"
    | (T id) -> Id.toString id
  let isObject = function
    | Object -> true
    | _ -> false
  let isRegion = function
    | Region -> true
    | _ -> false
end

module Type =
struct
  type t = Int | Bool | Object | Unit
         | ConApp of Tycon.t * t list
         | Tyvar of Tyvar.t
         | Unknown | Any

  let mkApp (x,y) = ConApp (x,y)
  let var v = Tyvar v
  let rec equal = function
    | (Int,Int) | (Bool,Bool) | (Object,Object) 
    | (Unit,Unit) | (Any,Any) -> true
    | (ConApp (tycon1,typs1), ConApp (tycon2,typs2)) -> 
        Tycon.equal (tycon1,tycon2) && 
        List.for_all2 (fun ty1 ty2 -> equal (ty1,ty2)) typs1 typs2
    | (Tyvar tyv1,Tyvar tyv2) -> Tyvar.equal (tyv1,tyv2)
    | _ -> false
  let rec mapTyvars f t = match t with
    | Int | Bool | Object | Unknown | Unit | Any -> t
    | Tyvar v -> f v
    | ConApp (tycon,tyargs) -> 
        ConApp (tycon,List.map (mapTyvars f) tyargs)
  let rec toString = function
    | Int -> "int" | Bool -> "bool" | Object -> "Object"
    | Unknown -> "?Unknown" | Unit -> "void" | Any -> "Any"
    | Tyvar v -> Tyvar.toString v
    | ConApp (tycon,tyargs) -> (Tycon.toString tycon)^
        begin
          if List.length tyargs = 0 then ""
          else "<"^(printCSV @@ List.map toString tyargs)^">"
        end 
  let isRegion = function
    | ConApp (Tycon.Region,_) -> true
    | _ -> false
end

module Prim =
struct
  type un_op = Not
  type bin_op = Plus | Minus | Mult | Div
              | GT | LT | And | Or | Equal
  let unOpToString = function | Not -> "!"
  let binOpToString = function
    | Plus -> "+" | Minus -> "-" | Mult -> "*" | Div -> "/"
    | GT -> ">" | LT -> "<" | And -> "&&" | Or -> "||" | Equal -> "=="
  let typOfBinOp op = 
    let boolt = ((Type.Bool,Type.Bool),Type.Bool) in
    let intt = ((Type.Int,Type.Int),Type.Int) in
    let intBoolt = ((Type.Int,Type.Int),Type.Bool) in
    let anyBoolt = ((Type.Any,Type.Any),Type.Bool) in
      match op with 
        | Plus | Minus | Mult | Div -> intt
        | GT | LT -> intBoolt | Equal -> anyBoolt
        | And | Or -> boolt
end

module Expr =
struct
  type node =
    Null
  | Int of int
  | Bool of bool
  | Var of Var.t
  | FieldGet of t * Field.t
  | MethodCall of t * MN.t * t list
  | New of Tycon.t * Type.t list * t list
  | UnOpApp of Prim.un_op * t
  | BinOpApp of t * Prim.bin_op * t
  and t = T of node * Type.t
  let node (T (n,_)) = n
  let typ (T (_,t)) = t
  let make (n,ty) = T (n,ty)
  let null = make (Null,Type.Any)
  let rec toString exp = 
    let node = node exp in
    let ty = typ exp in
    let tyAnnot str = "("^str^":"^(Type.toString ty)^")" in
    let doIt e = toString e in
      match node with 
        | Null -> tyAnnot @@ "Null"
        | Int i -> tyAnnot @@ string_of_int i
        | Bool b -> tyAnnot @@ string_of_bool b
        | Var v -> tyAnnot @@ Var.toString v
        | FieldGet (e,f) -> tyAnnot @@ (doIt e)^"."
                            ^(Field.toString f)
        | MethodCall (e,mn,argExps) -> tyAnnot @@
           let eStr = doIt e in
           let mnStr = MN.toString mn in
           let argStr = "("^(printCSV @@ List.map doIt argExps) ^")" in
             eStr^"."^mnStr^argStr
        | New (tycon,tyargs,argExps) -> tyAnnot @@
           let tyStr = Type.toString @@ Type.mkApp (tycon,tyargs) in
           let argStr = "("^(printCSV @@ List.map doIt argExps) ^")" in
             "new "^tyStr^argStr
        | UnOpApp (op,e) -> tyAnnot @@ (Prim.unOpToString op)^(doIt e)
        | BinOpApp (e1,op,e2) -> tyAnnot @@ (doIt e1)^" "
                                ^(Prim.binOpToString op)^" "^(doIt e2)
end

module Stmt = 
struct
  type t = 
      VarDec of Type.t * Var.t * Expr.t
    | Assn of Var.t * Expr.t
    | FieldSet of Expr.t * Expr.t
    | Expr of Expr.t
    | Seq of t list
    | ITE of Expr.t * t * t
    | While of Expr.t * t
    | LetRegion of t
    | Open of Expr.t * t
    | OpenAlloc of Expr.t * t
  
  let dec (t,v,e) = VarDec (t,v,e)
  let assn (v,e) = Assn (v,e)
  let expr e = Expr e
  let seq es = Seq es
  let rec print stmt = 
    let estr = Expr.toString in
    let tstr = Type.toString in
    let vstr = Var.toString in
      match stmt with
        | VarDec (ty,v,e) -> printf "%s" @@ (tstr ty)^" "
              ^(vstr v)^" = "^(estr e)^";"
        | Assn (v,e) -> printf "%s" @@ (vstr v)^" = "
                          ^(estr e)^";"
        | FieldSet (e1,e2) -> printf "%s" @@ (estr e1)^" = "
                              ^(estr e2)^";"
        | Expr e -> printf "%s" @@ estr e
        | Seq stmts -> 
            begin
              printf "@[<v 2>";
              List.iter (fun stmt -> 
                begin
                  printf "@\n";
                  print stmt;
                end) stmts;
              printf "@]";
            end
        | ITE (grd,tstmt,fstmt) ->
            begin
              printf "if (%s) {" (estr grd);
              printf "@\n";
              printf "@[<v 2>"; print tstmt; printf "@]";
              printf "}";
              printf "else {";
              printf "@\n";
              printf "@[<v 2>"; print fstmt; printf "@]";
              printf "}"
            end
        | While (grd,stmt) -> 
            begin
              printf "while (%s) {" (estr grd);
              printf "@\n";
              printf "@[<v 2>"; print stmt; printf "@]";
              printf "}";
            end 
        | LetRegion stmt ->
            begin
              printf "letregion {";
              printf "@\n";
              printf "@[<v 2>"; print stmt; printf "@]";
              printf "}";
            end
        | Open (e,stmt) ->
            begin
              printf "open (%s) {" (estr e);
              printf "@\n";
              printf "@[<v 2>"; print stmt; printf "@]";
              printf "}";
            end
        | OpenAlloc (e,stmt) ->
            begin
              printf "openalloc (%s) {" (estr e);
              printf "@\n";
              printf "@[<v 2>"; print stmt; printf "@]";
              printf "}";
            end
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
  let print m = 
    let retTyStr = Type.toString @@ ret_type m in
    let mnStr = MN.toString @@ name m in
    let params = params m in
    let paramStr = printCSV @@ List.map 
       (fun (v,ty) -> (Type.toString ty)^" "^(Var.toString v)) params in
    let body = body m in
      begin
        printf "%s" @@ retTyStr^" "^mnStr^"("^paramStr^") {";
        printf "@\n";
        printf "@[<v 2>";
        Stmt.print body;
        printf "@]";
        printf "@\n";
        printf "}"
      end
end

module MakeCon = functor(S : sig
                               module Type : STRINGABLE
                               module Stmt: PRINTABLE
                             end) ->
struct
  open S
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

  let print c = 
    let tycon = tycon c in
    let params = params c in
    let paramStr = printCSV @@ List.map 
       (fun (v,ty) -> (Type.toString ty)^" "^(Var.toString v)) params in
    let body = body c in
      begin
        printf "%s" @@ (Tycon.toString tycon)^" ("^paramStr^") {";
        printf "@\n";
        printf "@[<v 2>";
        Stmt.print body;
        printf "@]";
        printf "@\n";
        printf "}";
      end 
end

module Con = MakeCon(struct
                       module Type = Type
                       module Stmt = Stmt
                     end)

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

  (*let ctor k argTys = 
    let validApp ctor = 
      let paramTys = List.map snd (Con.params ctor) in
        andOf [
          List.length paramTys = List.length argTys;
          List.for_all2 (fun argExpTy paramTy -> 
                isSubType ct tyVE (argExpTy,paramTy)) 
            argTys paramTys
        ] in
    let ctors = Class.ctors k in
      List.find validApp ctors*)

  let print k = 
    let className = Tycon.toString @@ tycon k in
    let tyvarDecs = tyvars k in
    let tyvarDectoStr (tyvar,ty) = (Tyvar.toString tyvar)
              ^" extends "^(Type.toString ty) in
    let tyvarDecsStr = if List.length tyvarDecs = 0 then ""
       else "<"^(printCSV @@ List.map tyvarDectoStr tyvarDecs)^">" in
    let classSig = className^tyvarDecsStr in
    let superSig = Type.toString @@ super k in
    let fdecs = fields k in
    let fdecToStr (f,ty) = (Type.toString ty)^" "^(Field.toString f)
                            ^";" in
    let cons = ctors k in
    let meths = methods k in
      begin
        printf "%s" @@ "class "^classSig^" extends "^superSig^" {";
        printf "@?";
        printf "@[<v 2>";
        List.iter (fun fdec -> 
          begin
            printf "@\n";
            printf "%s" @@ fdecToStr fdec;
          end) fdecs;
        (* ctors *)
        List.iter (fun con ->
          begin
            printf "@\n";
            Con.print con;
          end) cons;
        (* methods *)
        List.iter (fun m ->
          begin
            printf "@\n";
            Method.print m;
          end) meths;
        printf "@]";
        printf "@\n";
        printf "}";
      end
end
