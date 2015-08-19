open Utils
open Format
open Id
open Ast

module MN = MethodName  

module RegionVar =
struct
  type typ = Rho | R | Dummy
  type t = T of typ ref * string 

  let freshId = mkUidGen ""
  let freshRho () = T (ref Rho, freshId ())
  let freshR () = T (ref R, freshId ())
  let dummy = T (ref Dummy,freshId ())
  let concretize = function
    | T ({contents=Dummy},_) -> 
        failwith "Dummy RV cannot be concretized"
    | T (x,id) -> (x := R)
  let equal (T (typ1,id1), T (typ2,id2)) = 
    (typ1.contents = typ2.contents) && (id1 = id2)
  let isDummy (T (typref,_)) = match !typref with
    | Dummy -> true | _ -> false
  let toString (T (typref,s)) = match !typref with
    | Rho -> "ρ"^s 
    | R -> "R"^s 
    | Dummy -> "--dummy--"
  let isConcrete (T (typref,s)) = match !typref with
    | R -> true | Rho -> false 
    | _ -> failwith "isConcrete called on a Dummy RV"
end

module RV = RegionVar

module RegionVarSet = Set.Make (struct
                                  type t = RegionVar.t
                                  let compare = compare
                                end)

module RegionConstraint =
struct
  type t =  True 
          | False
          | Outlives of RegionVar.t * RegionVar.t 
          | Eq of RegionVar.t * RegionVar.t 
          | NotEq of RegionVar.t * RegionVar.t
          | NotOutlives of RegionVar.t * RegionVar.t
          | Conj of t list
          | Disj of t list

  let rec toString phi =
    let f = RV.toString in 
    let g r1 rel r2 = (f r1)^rel^(f r2) in
    let ret str = "("^str^")" in
      match phi with
        | True -> "T"
        | False -> "⊥"
        | Outlives (r1,r2) -> g r1 "≥" r2
        | Eq (r1,r2) -> g r1 "=" r2
        | NotEq (r1,r2) -> g r1 "≠" r2
        | NotOutlives (r1,r2) -> "¬"^(g r1 "≥" r2)
        | Conj phis -> ret @@ printSV " ∧ " @@ List.map toString phis
        | Disj phis -> ret @@ printSV " ∨ " @@ List.map toString phis

  let truee = True

  let falsee = False

  let equal (rho1,rho2) = 
    if RegionVar.equal (rho1,rho2) then True else Eq (rho1,rho2)

  let notEqual (rho1,rho2) = 
    if RegionVar.equal (rho1,rho2) then False else NotEq (rho1,rho2)

  let conj l = 
    (*
    let raw_bt = Printexc.get_callstack 5 in
    let _ = 
      begin
        print_string "conj backtrace:\n";
        print_string @@ Printexc.raw_backtrace_to_string raw_bt;
        print_string "\n"
      end in
     *)
    let l' = List.fold_right 
               (fun prop props -> match prop with
                  | Conj props' -> List.append props' props
                  | _ -> prop::props) l [] in
      if List.existsEq False l' 
      then failwith @@ toString @@ Conj l (*False *)
      else match List.filterNotEq True l' with 
            | [] -> True | l'' -> Conj l''

  let disj l = 
    let l' = List.fold_right 
               (fun prop props -> match prop with
                  | False -> props
                  | Disj props' -> List.append props' props
                  | _ -> prop::props) l [] in
      if List.existsEq True l' then True
      else match l' with 
            | [] -> True | [p] -> p | _ -> Disj l'

  let outlives (rhoLong,rhoShort) = Outlives (rhoLong,rhoShort)

  let inSet (rho,rhoSet) = 
    if RegionVarSet.mem rho rhoSet then True
    else Disj (List.map (fun rho' -> Eq (rho,rho'))
                (RegionVarSet.elements rhoSet))

  let notInSet (rho,rhoSet) = 
    if RegionVarSet.mem rho rhoSet then False
    else Conj (List.map (fun rho' -> NotEq (rho,rho'))
                (RegionVarSet.elements rhoSet))

  let subEq (s1,s2) = 
    if RegionVarSet.subset s1 s2 then True
    else let l1 = RegionVarSet.elements s1 in
            conj @@ List.map (fun rho1 -> inSet (rho1,s2)) l1

  let rec mapRegionVars f = function
    | Outlives (r1,r2) -> Outlives (f r1,f r2)
    | Eq (r1,r2) -> equal (f r1,f r2)
    | NotEq (r1,r2) -> notEqual (f r1,f r2)
    | NotOutlives (r1,r2) -> notEqual (f r1,f r2)
    | Conj props -> conj @@ List.map (mapRegionVars f) props
    | Disj props -> disj @@ List.map (mapRegionVars f) props
    | prop -> prop

  let doSubst = mapRegionVars
end

module Phi = RegionConstraint

module ConstraintSolve = 
struct
  module CS = ConstraintSolver.Make (struct
                                       module Symbol = RegionVar
                                       module Constraint = 
                                       struct
                                         type symbol_t = RegionVar.t
                                         include RegionConstraint
                                         let mapSymbols = mapRegionVars
                                       end
                                     end)
  include CS
end

module Type =
struct
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

  let mkApp (tycon,rAlloc,rBar,tyArgs) = 
    ConApp {tycon=tycon; rAlloc=rAlloc; rBar=rBar; tyArgs=tyArgs;}

  let rec mapTyvars f t = match t with
    | Int | Bool | Object _ | Unknown | Unit | Any -> t
    | Tyvar v -> f v
    | ConApp args -> ConApp {args with 
                      tyArgs = List.map (mapTyvars f) args.tyArgs} 
    | Region args -> Region {args with 
                                 rootObjTy = mapTyvars f args.rootObjTy}
    | Exists (rho,t') -> Exists (rho, mapTyvars f t')

  let rec mapRegionVars f t = 
    let doIt = mapRegionVars f in
      match t with 
          Object r -> Object (f r)
        | ConApp args -> ConApp {args with rAlloc = f args.rAlloc; 
                                  rBar = List.map f args.rBar;
                                  tyArgs = List.map doIt args.tyArgs}
        | Region {rho;rhoAlloc;rootObjTy} -> Region {rho = f rho;
                                             rhoAlloc = f rhoAlloc;
                                             rootObjTy = doIt rootObjTy;}
        | Exists (boundRho,t') ->
            let f' = fun rho -> if RegionVar.equal (rho,boundRho) 
                                then boundRho else f rho in
              Exists (boundRho, mapRegionVars f' t')
        | _ -> t

  let rec frv = function
    | Object r -> [r]
    | ConApp args -> 
      (*
       * Relying on informal invariant that dummy rho only occurs as 
       * allocation argument to ConApp.
       *)
        let rest = List.concat [args.rBar; frvStar args.tyArgs] in
          if RV.isDummy args.rAlloc then rest else args.rAlloc::rest
    | Region args -> args.rho :: args.rhoAlloc :: (frv args.rootObjTy)
    | Exists (boundRho,t') -> List.filterNotEq boundRho (frv t')
    | _ -> []
  and frvStar ts = List.concat @@ List.map frv ts

  let var tyvar = Tyvar tyvar

  let rec map f t = match (f t,t) with
    | (Some t',_) -> t'
    | (None, ConApp args) -> 
        let tyargs' = List.map (map f) args.tyArgs in
        let args' = {args with tyArgs = tyargs'} in
          ConApp args'
    | (None, Region args) ->
        let rootObjTy' = map f args.rootObjTy in
        let args' = {args with rootObjTy=rootObjTy'} in
          Region args'
    | (None, Exists (rho,rTy)) -> Exists (rho,map f rTy)
    | (None,_) -> t

  let rec toString = function
    | Int -> "int" | Bool -> "bool" | Unknown -> "?Unknown" 
    | Unit -> "void" | Any -> "Any"
    | Object rho -> "Object<"^(RV.toString rho)^">"
    | Tyvar v -> Tyvar.toString v
    | ConApp args -> (Tycon.toString args.tycon)^"<"^
        begin
          printCSV @@ List.map RV.toString @@ args.rAlloc::args.rBar
        end^">"^
        begin
          if List.length args.tyArgs = 0 then ""
          else "<"^(printCSV @@ List.map toString args.tyArgs)^">"
        end
    | Region {rho;rhoAlloc;rootObjTy} -> 
        "Region["^(RV.toString rho)^"]<"^(RV.toString rhoAlloc)^"><"
          ^(toString rootObjTy)^">"
    | Exists (rho,t) -> "∃"^(RV.toString rho)^"."^(toString t)

  let rec erase = let module ATy = Ast.Type in function
    | Int -> ATy.Int | Bool -> ATy.Bool | Unknown -> ATy.Unknown
    | Any -> ATy.Any | Unit -> ATy.Unit | Tyvar v -> ATy.Tyvar v
    | Object _ -> ATy.Object 
    | ConApp {tycon;tyArgs} -> ATy.ConApp (tycon, List.map erase tyArgs)
    | Region {rootObjTy} -> 
        ATy.ConApp (Ast.Tycon.make @@ Id.fromString "Region", 
                    [erase rootObjTy])
    | Exists (_,t) -> erase t

  let rec equal = function 
    | (Int,Int) | (Bool,Bool) | (Unit,Unit) | (Any,Any) -> Phi.truee
    | (Tyvar v1, Tyvar v2) -> if Tyvar.equal (v1,v2) 
                                then Phi.truee else Phi.falsee
    | (Object rho1, Object rho2) -> Phi.equal (rho1,rho2)
    | (ConApp args1,ConApp args2) -> 
        if Tycon.equal (args1.tycon, args2.tycon) then
          let c1 = Phi.equal (args1.rAlloc,args2.rAlloc) in
          let c2s = List.map2 (curry Phi.equal) 
                      args1.rBar args2.rBar in
          let c3s = List.map2 (curry equal)
                      args1.tyArgs args2.tyArgs in
            Phi.conj @@ List.concat [[c1]; c2s; c3s]
        else
            Phi.falsee
    | (Region args1,Region args2) -> 
        Phi.conj [Phi.equal (args1.rho, args2.rho);
                  Phi.equal (args1.rhoAlloc, args2.rhoAlloc);
                  equal (args1.rootObjTy,args2.rootObjTy)]
    | (Exists (rho1,t1), Exists (rho2,t2)) ->
        let substFn = fun rho -> if RegionVar.equal (rho,rho2) 
                                    then rho1 else rho in
        let t2' = mapRegionVars substFn t2 in
          equal (t1,t2')
    | _ -> Phi.falsee 
end

module Expr =
struct
  type method_call_t = {meth: t * MethodName.t;
                        rAlloc: RegionVar.t;
                        rBar: RegionVar.t list;
                        args: t list}
  and node =
    | Null
    | Int of int
    | Bool of bool
    | Var of Var.t 
    | FieldGet of t * Field.t
    | MethodCall of method_call_t
    | New of Type.t * t list
    | UnOpApp of Prim.un_op * t
    | BinOpApp of t * Prim.bin_op * t
    | Pack of RegionVar.t * t
  and t = T of node * Type.t
  let typ (T (_,t)) = t
  let node (T (n,_)) = n
  let make (n,ty) = T (n,ty)
  let rec mapRegionVars f e = 
    let doIt = mapRegionVars f in
    let doItTy = Type.mapRegionVars f in
    let ret n = make (n, typ e) in
      match node e with
        | Null | Int _ | Bool _ | Var _ -> e
        | FieldGet (e,f) -> ret @@ FieldGet (doIt e,f)
        | MethodCall x -> ret @@ MethodCall 
                               {x with rAlloc = f x.rAlloc;
                                       rBar = List.map f x.rBar;
                                       args = List.map doIt x.args}
        | New (t,args) -> ret @@ New (doItTy t, List.map doIt args)
        | UnOpApp (unop,e) -> ret @@ UnOpApp (unop, doIt e)
        | BinOpApp (e1,binop,e2) -> ret @@ BinOpApp (doIt e1,binop,doIt e2)
        | Pack (rho,e') -> 
            let f' = fun rho' -> if RegionVar.equal (rho',rho)
                                  then rho else f rho in
              ret @@ Pack(rho, mapRegionVars f' e')

  let rec toString (T (node,ty)) = 
    "("^(nodeToString node)^":"^(Type.toString ty)^")"
  and nodeToString = function
    | Null -> "Null" | Int i -> string_of_int i
    | Bool b -> string_of_bool b | Var v -> Var.toString v
    | FieldGet (e,f) -> (toString e)^"." ^(Field.toString f) 
    | MethodCall {meth = (objExp,mn); rAlloc; rBar; args} -> 
        let methStr = (toString objExp)^"."^(MN.toString mn) in
        let rargs = rAlloc :: rBar in
        let rargsStr = printCSV @@ List.map RV.toString rargs in
        let argsStr = printCSV @@ List.map toString args in
          methStr^"<"^rargsStr^">"^"("^argsStr^")"
    | New (ty,args) -> "new "^(Type.toString ty)^"("
                           ^(printCSV @@ List.map toString args)^")"
    | UnOpApp (op,e) -> (Prim.unOpToString op)^(toString e)
    | BinOpApp (e1,op,e2) -> (toString e1)^" " ^(Prim.binOpToString op)
                              ^" "^(toString e2)
    | Pack (rho,e') -> 
        let tyStr = Type.toString @@ Type.Exists (rho,typ e') in
        let e'Str = toString e' in
          "pack "^e'Str^" as "^tyStr

end

module Stmt =
struct
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

  let rec mapRegionVars f t = 
    let doIt = mapRegionVars f in
    let doItTy = Type.mapRegionVars f in
    let doItExp = Expr.mapRegionVars f in 
      match t with
        | VarDec (ty,v,e) -> VarDec (doItTy ty, v, doItExp e)
        | Assn (v,e) -> Assn (v,doItExp e)
        | FieldSet (e1,e2) -> FieldSet (doItExp e1, doItExp e2) 
        | Expr e -> Expr (doItExp e)
          (* Note: The following simple substitution for Seq is
           * unsound, as an UnpackDec stmt can introduce a new rho, 
           * which should not be substituted in subsequent stmts. 
           * We are ignoring this for now *)
        | Seq ts -> Seq (List.map doIt ts)
        | LetRegion (rho,stmt) -> 
            mapRegionVars (fun rho' -> 
                             if RegionVar.equal (rho,rho') 
                             then rho else f rho') stmt
        | Open (e,stmt) -> Open (doItExp e, doIt stmt)
        | OpenAlloc (e,stmt) -> OpenAlloc (doItExp e, doIt stmt)
        | UnpackDec args -> UnpackDec {args with ty = doItTy args.ty;
                                  unpackExp = doItExp args.unpackExp}

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
        | LetRegion (rho,stmt) ->
            begin
              printf "%s" @@ "letregion<"^(RV.toString rho)^"> {";
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
        | UnpackDec {rho; ty; var; unpackExp = e;} -> printf "%s" @@
              "let ("^(RV.toString rho)^","^(Type.toString ty)^" "
            ^ (Var.toString var)^") = unpack "^(estr e)^";"
end

module Method =
struct
  type t = {
    name     : MN.t;
    rhoAlloc : RegionVar.t;
    rhoBar   : RegionVar.t list;
    phi      : RegionConstraint.t;
    params   : (Var.t * Type.t) list;
    body     : Stmt.t;
    ret_type : Type.t;
  }
  let make ~name ~rhoAlloc ~rhoBar ~phi 
           ~params ~body ~ret_type = 
    {
      name     = name;
      rhoAlloc = rhoAlloc;
      rhoBar   = rhoBar;
      phi      = phi;
      params   = params;
      body     = body;
      ret_type = ret_type;
    }
  let name m = m.name
  let rhoAlloc m = m.rhoAlloc
  let rhoBar m = m.rhoBar
  let phi m = m.phi
  let params m = m.params
  let body m = m.body
  let ret_type m = m.ret_type

  let print m = 
    let retTyStr = Type.toString @@ ret_type m in
    let mnStr = MN.toString @@ name m in
    let rhoAlloc = rhoAlloc m in
    let rhoBar = rhoBar m in
    let rhoStr = printCSV @@ List.map RV.toString @@ 
                   rhoAlloc::rhoBar in
    let phiStr = RegionConstraint.toString @@ phi m in
    let mSig = "<"^rhoStr^"|"^phiStr^">"^mnStr in
    let params = params m in
    let paramStr = printCSV @@ List.map 
       (fun (v,ty) -> (Type.toString ty)^" "^(Var.toString v)) params in
    let body = body m in
      begin
        printf "%s" @@ retTyStr^" "^mSig^"("^paramStr^") {";
        printf "@\n";
        printf "@[<v 2>";
        Stmt.print body;
        printf "@]";
        printf "@\n";
        printf "}"
      end
end

module Con = MakeCon(struct
                       module Type = Type
                       module Stmt = Stmt
                     end)
module Class =
struct
  type t = {
    tycon    : Tycon.t;
    rhoAlloc : RegionVar.t;
    rhoBar   : RegionVar.t list;
    phi      : RegionConstraint.t;
    tyvars   : (Tyvar.t * Type.t) list;
    super    : Type.t;
    fields   : (Field.t * Type.t) list;
    ctors    : Con.t list;
    methods  : Method.t list;
  }

  let make ~tycon ~rhoAlloc ~rhoBar ~phi ~tyvars 
           ~super ~fields ~ctors ~methods
    = { tycon = tycon;
        rhoAlloc = rhoAlloc;
        rhoBar   = rhoBar;
        phi      = phi;
        tyvars = tyvars;
        super   = super;
        fields  = fields;
        ctors   = ctors;
        methods = methods }

  let tycon k = k.tycon
  let rhoAlloc k = k.rhoAlloc
  let rhoBar k = k.rhoBar
  let phi k = k.phi
  let tyvars k = k.tyvars
  let super k = k.super
  let fields k = k.fields
  let ctors k = k.ctors
  let methods k = k.methods

  let print k = 
    let className = Tycon.toString @@ tycon k in
    let rhoAlloc = rhoAlloc k in
    let rhoBar = rhoBar k in
    let rhoStr = printCSV @@ List.map RV.toString @@ 
                   rhoAlloc::rhoBar in
    let phiStr = RegionConstraint.toString @@ phi k in
    let tyvarDecs = tyvars k in
    let tyvarDectoStr (tyvar,ty) = (Tyvar.toString tyvar)
              ^" extends "^(Type.toString ty) in
    let tyvarDecsStr = if List.length tyvarDecs = 0 then ""
       else "<"^(printCSV @@ List.map tyvarDectoStr tyvarDecs)^">" in
    let classSig = className^"<"^rhoStr^"|"^phiStr^">"^tyvarDecsStr in
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
        printf "@]"; printf "@\n}"; printf "@\n@?";
      end
end
