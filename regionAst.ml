open Utils
open Format
open Id
open Ast

module MN = MethodName  

module RegionVar =
struct
  type t = string

  let fresh = mkUidGen "R"
  let dummy = mkUidGen "--dummy--" ()
  let equal (t1,t2) = t1 = t2
  let isDummy t = equal (t,dummy)
  let toString t = t
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
          | Conj of t list
          | Disj of t list

  let rec toString phi =
    let f = RV.toString in 
    let g r1 rel r2 = (f r1)^rel^(f r2) in
      match phi with
        | True -> "T"
        | False -> "⊥"
        | Outlives (r1,r2) -> g r1 "≥" r2
        | Eq (r1,r2) -> g r1 "=" r2
        | NotEq (r1,r2) -> g r1 "≠" r2
        | Conj phis -> printSV " ∧ " @@ List.map toString phis
        | Disj phis -> printSV " ∨ " @@ List.map toString phis

  let truee = True

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
      if List.existsEq False l' then False
      else match List.filterNotEq True l' with 
            | [] -> True | l'' -> Conj l''

  let disj l = 
    let l' = List.fold_right 
               (fun prop props -> match prop with
                  | Disj props' -> List.append props' props
                  | _ -> prop::props) l [] in
      if List.existsEq True l' then True
      else match List.filterNotEq False l with 
            | [] -> True | l'' -> Disj l''

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
    | Conj props -> conj @@ List.map (mapRegionVars f) props
    | Disj props -> disj @@ List.map (mapRegionVars f) props
    | prop -> prop

  let doSubst = mapRegionVars
end

module ConstraintSolve = 
struct
  module CS = ConstraintSolver.Make (struct
                                       module Symbol = RegionVar
                                       module Constraint = 
                                       struct
                                         type symbol_t = RegionVar.t
                                         include RegionConstraint
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
end

module Expr =
struct
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
end

module Con = MakeCon(struct
                       include Type
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
        (*
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
         *)
        printf "@\n";
        printf "}";
      end
end
