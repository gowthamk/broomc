module type TYPE_CHECK_STRUCTS = 
sig
  module CT : Map.S with type key = Ast.Tycon.t
end
module Make (S:TYPE_CHECK_STRUCTS) =
struct
  open S
  open Ast
  open Utils
  open Envs
  open Printf
  module L = List
  module S = Set.Make (struct
                        type t = int
                        let compare = compare
                       end)
  module MN = MethodName

  exception Type_error of string

  type arrow_typ = Arrow of Type.t list * Type.t

  let failwith msg = raise (Type_error msg)

  let isTopTyp = function 
    | Type.Object -> true
    | Type.ConApp _ -> false
    | typ -> failwith @@ "isTopTyp: "^(Type.toString typ)
              ^" is not a class type.\n"

  (*
   * Unifies tyargs to tycon with formal tyvars, and generates a 
   * substitution function. 
   *)
  let unifyTyconArgs ct tycon tyargs = 
    let invalidConAppMsg = "Invalid instantiation of generic \
           \ class "^(Tycon.toString tycon)^"\n" in
    let k = (try CT.find tycon ct with
      | Not_found -> failwith @@ "Unknown tycon: "
                            ^(Tycon.toString tycon)^"\n") in
    let tyvarDecs = Class.tyvars k in
    let _ = 
      begin
        assrt (List.length tyargs = List.length tyvarDecs,
               invalidConAppMsg);
      end in
    let tyvars = List.map fst tyvarDecs in
    let substs = List.combine tyvars tyargs in
    let substfn = 
      fun tyvar -> 
        try List.assoc tyvar substs with
          | Not_found -> failwith @@ "Unknown tyvar: "
                                  ^(Tyvar.toString tyvar) in
      substfn

  let superOfClassTy ct classTyp = 
    let (tycon,tyargs) = match classTyp with 
      | Type.ConApp (x,y) -> (x,y)
      | Type.Object -> failwith @@ "superOfClassTy: super class of \
                                     \ Object doesn't exist."
      | _ -> failwith @@ "superOfClassTy: "^(Type.toString classTyp)
                        ^" is not a class type." in
    let substfn = unifyTyconArgs ct tycon tyargs in
    let k = CT.find tycon ct in 
    let formalSuper = Class.super k in
    let actualSuper = Type.mapTyvars substfn formalSuper in
      actualSuper

  let rec isSubtype ct tyVE (subTyp,supTyp) = 
    Type.equal (subTyp,supTyp) ||
    match (subTyp,supTyp) with 
      | (Type.Any,_) -> true | (_,Type.Any) -> true
        (* A tyvar cannot be subtype of another tyvar.
         * If 'a <: A and 'b <: B and B <: A, then we cannot derive 
         * 'b <: 'a. *)
      | (Type.Tyvar v,_) -> isSubtype ct tyVE (TyVE.find v tyVE, supTyp)
      | (Type.Object,_) -> false
      | (Type.ConApp _,_) -> 
          let superOfSubTyp = superOfClassTy ct subTyp in
            isSubtype ct tyVE (superOfSubTyp,supTyp)
      | _ -> false

  let rec subtypeOk ct tyVE (subTyp,supTyp) = 
    assrt (isSubtype ct tyVE (subTyp,supTyp), 
           "Invalid: "^(Type.toString subTyp)^" <: "
                         ^(Type.toString supTyp)^"\n")

  let fnSubTypOK ct tyVE (Arrow (argTys1,retTy1),
                          Arrow (argTys2,retTy2)) =
    begin
      assrt (List.length argTys1 = List.length argTys2,
             "Invalid method overriding\n");
      (*
       * Fn subtyping is invariant in argTys, and covariant in retTys.
       *)
      List.iter2 (fun argTy1 argTy2 -> assrt (Type.equal (argTy1,argTy2),
                      "Invalid method overriding\n")) 
        argTys1 argTys2;
      subtypeOk ct tyVE (retTy1, retTy2);
    end

  let rec typeOk ct tyVE typ = match typ with
    | Type.Any | Type.Int | Type.Bool | Type.Object | Type.Unit -> ()
    | Type.Unknown -> failwith "Unknown type is not ok\n"
    | Type.Tyvar tyvar -> assrt (TyVE.mem tyvar tyVE,
                          "Unknown tyvar: "^(Tyvar.toString tyvar))
    | Type.ConApp (tycon,tyargs) -> 
        let _ = 
          begin
            List.iter (typeOk ct tyVE) tyargs
          end in
        let k = (try CT.find tycon ct with
          | Not_found -> failwith @@ "Unknown tycon: "
                                ^(Tycon.toString tycon)^"\n") in
        let (tyvars,tyvarBounds) = List.split (Class.tyvars k) in
        let substfn = unifyTyconArgs ct tycon tyargs in
        let tyvarBounds' = List.map (Type.mapTyvars substfn) 
                             tyvarBounds in
        let _ = List.iter2 (fun tyarg tyvarBound' -> 
                              subtypeOk ct tyVE (tyarg,tyvarBound'))
                  tyargs tyvarBounds' in
          ()
  (*
   * What is the type of the field classTyp.f ?
   *)
  let rec ftype ct (classTyp,f) =
    let _ = if isTopTyp classTyp then 
      begin
        print_string @@ "Feild "^(Field.toString f)^" not found.\n";
        raise Not_found;
      end
    else () in
    let (tycon,tyargs) = match classTyp with 
      | Type.ConApp (x,y) -> (x,y)
      | _ -> failwith @@ "ftype: "^(Type.toString classTyp)
                        ^" is not a class type\n" in
    let substfn = unifyTyconArgs ct tycon tyargs in
    let k = CT.find tycon ct in 
    let formalSuper = Class.super k in
    let actualSuper = Type.mapTyvars substfn formalSuper in
    let fieldDecs = Class.fields k in
      try 
        List.find (fun (f',ty) -> Field.equal (f,f')) 
          fieldDecs |> snd |> Type.mapTyvars substfn
      with Not_found -> ftype ct (actualSuper,f)
  (*
   * What is the type of method classTyp.mname ?
   *)
  exception Return of arrow_typ 
  let rec mtype ct (classTyp,mname) = 
    try
      let _ = if isTopTyp classTyp then raise Not_found else () in
      let (tycon,tyargs) = match classTyp with 
        | Type.ConApp (x,y) -> (x,y)
        | _ -> failwith @@ "mtype: "^(Type.toString classTyp)
                          ^" is not a class type\n" in
      let substfn = unifyTyconArgs ct tycon tyargs in
      let k = CT.find tycon ct in 
      let formalSuper = Class.super k in
      let actualSuper = Type.mapTyvars substfn formalSuper in
      let meths = Class.methods k in
      let m = 
        try 
          List.find (fun m -> MN.equal (Method.name m,mname)) meths 
        with Not_found -> raise @@ Return (mtype ct (actualSuper,mname)) in 
      let actualArgTys = List.map (fun (_,ty) -> 
                                 Type.mapTyvars substfn ty) 
                       (Method.params m) in
      let actualRetTy = Type.mapTyvars substfn (Method.ret_type m) in
        Arrow (actualArgTys,actualRetTy)
    with | Return x -> x

  let rec typeOfStmt stmt = 
    let open Stmt in 
      match stmt with
        | Expr e -> Expr.typ e
        | Seq [] -> Type.Unit
        | Seq stmts -> typeOfStmt @@ List.nth stmts 
                                       (List.length stmts - 1)
        | _ -> Type.Unit

  let typeOfUnOpApp (op,e) = match op with
    | Prim.Not -> 
        let _ = assrt (Type.equal (Expr.typ e,Type.Bool),
                     "`Not` operator received a non-boolean value") in
          Type.Bool

  let typeOfBinOpApp (op,e1,e2) = 
    let (ety1,ety2) = (Expr.typ e1,Expr.typ e2) in
    let ((ty1,ty2),ty) = Prim.typOfBinOp op in
    let opStr = Prim.binOpToString op in
      begin
        assrt(Type.equal (ety1,ty1) && Type.equal (ety2,ty2),
              opStr^" argument type mismatch");
        ty
      end

  let rec elabExpr ct (tyVE,ve) expr =
    let open Expr in 
    let doIt e = elabExpr ct (tyVE,ve) e in
    let typeOfVar v = try VE.find v ve 
        with Not_found -> 
          begin
            print_string "Var Env is:\n";
            VE.iter (fun v ty -> 
              print_string @@ (Var.toString v)^" : "
                ^(Type.toString ty)^"\n") ve;
            failwith @@ "Variable "^(Var.toString v)
                                      ^" undeclared\n";
          end in
    let expNode = node expr in
      match expNode with 
        | Null -> make (expNode,Type.Any)
        | Int _ -> make (expNode,Type.Int)
        | Bool _ -> make (expNode,Type.Bool)
        | Var v -> make (expNode,typeOfVar v)
        | FieldGet (e,f) -> 
            let e' = doIt e in
            let classTyp = Expr.typ e' in
              make (FieldGet (e',f), ftype ct (classTyp,f))
        | MethodCall (objExp,mname,argExps) -> 
            let objExp' = doIt objExp in
            let argExps' = List.map doIt argExps in
            let classTyp = Expr.typ objExp' in
            let argExpTyps = List.map Expr.typ argExps' in
            let errMsg = "Invalid application of method: "
              ^(Type.toString classTyp)^"."^(MN.toString mname)^"\n" in
            let Arrow (argTyps,retTyp) = mtype ct (classTyp,mname) in
              begin
                assrt (List.length argExpTyps = List.length argTyps,
                       errMsg);
                (*
                 * argExpTyps <: argTyps
                 *)
                List.iter2 (fun argExpTy argTy -> 
                              subtypeOk ct tyVE (argExpTy,argTy)) 
                  argExpTyps argTyps;
                make (MethodCall (objExp',mname,argExps'),retTyp);
              end
        | New (tycon,tyargs,argExps) -> 
            let classTyp = Type.mkApp (tycon,tyargs) in
            let _ = typeOk ct tyVE classTyp in 
            let argExps' = List.map doIt argExps in
            let argExpTyps = List.map Expr.typ argExps' in
            let k = CT.find tycon ct in 
            let validApp ctor = 
              let paramTys = List.map snd (Con.params ctor) in
                andOf [
                  List.length paramTys = List.length argExpTyps;
                  List.for_all2 (fun argExpTy paramTy -> 
                        isSubtype ct tyVE (argExpTy,paramTy)) 
                    argExpTyps paramTys
                ] in
            let ctors = Class.ctors k in
              begin
                assrt (List.exists validApp ctors,
                       "Invalid `new "^(Type.toString classTyp)^"`\n");
                make (New (tycon,tyargs,argExps'),classTyp)
              end
        | UnOpApp (op,e) -> 
            let e' = doIt e in
            let ty = typeOfUnOpApp (op,e') in
              make (UnOpApp (op,e'),ty)
        | BinOpApp (e1,op,e2) ->
            let (e1',e2') = (doIt e1, doIt e2) in
            let ty = typeOfBinOpApp (op,e1',e2') in
              make (BinOpApp (e1',op,e2'),ty)


  let rec elabStmt ct (tyVE,ve) stmt =
    let open Stmt in 
    let doIt s = elabStmt ct (tyVE,ve) s in
    let doItExp e = elabExpr ct (tyVE,ve) e in
    let ret stmt = (stmt,ve) in
      match stmt with 
        | VarDec (varTy,v,e) -> 
            let e' = doItExp e in
            let expTy = Expr.typ e' in
            let _ = subtypeOk ct tyVE (expTy,varTy) in
            let ve' = VE.add v varTy ve in 
              (VarDec (varTy,v,e'),ve')
        | Assn (v,e) -> ret @@ Assn (v,doItExp e)
        | FieldSet (e1,e2) -> ret @@ FieldSet (doItExp e1, doItExp e2)
        | Expr e -> ret @@ Expr (doItExp e)
        | Seq stmts -> 
            let (stmts',ve') = List.fold_left 
                 (fun (stmts',ve) stmt -> 
                    let (stmt',ve') = elabStmt ct (tyVE,ve) stmt in
                      (List.append stmts' [stmt'],ve')) 
                 ([],ve) stmts in
              (Seq stmts',ve')
        | LetRegion s -> 
            let (s',ve') = doIt s in
              (LetRegion s',ve')
        | Open (vexp,s) -> 
            let vexp' = doItExp vexp in
            let ty = Expr.typ vexp' in
            let _ = assrt (Type.isRegion ty, "A variable of non-\
                           \region type cannot be opened.") in
            let (s',ve') = doIt s in
              (Open (vexp',s'),ve')
        | OpenAlloc (vexp,s) -> 
            let vexp' = doItExp vexp in
            let ty = Expr.typ vexp' in
            let _ = assrt (Type.isRegion ty, "A variable of non-\
                           \region type cannot be openalloc'd.") in
            let (s',ve') = doIt s in
              (OpenAlloc (vexp',s'),ve')

  let overrideOk ct tyVE (mname,classTyp, mtyp) =
    match classTyp with 
      | Type.Object -> ()
      | Type.ConApp _ -> 
          let supTyp = superOfClassTy ct classTyp in
          let mtypInSuper = mtype ct (supTyp,mname) in
            fnSubTypOK ct tyVE (mtyp,mtypInSuper)
      | _ -> failwith @@ "overrideOk: "^(Type.toString classTyp)
                           ^" not a classtype."

  (*
   * Besides elborating the method body, the function also checks 
   *  meth OK in tycon 
   *)
  let elabMethod ct (tyVE,thisVE) meth tycon =
    let name = Method.name meth in
    let params = Method.params meth in
    let argTyps = List.map snd params in
    let retTyp = Method.ret_type meth in
    let _ = 
      begin
        List.iter (typeOk ct tyVE) argTyps;
        typeOk ct tyVE retTyp;
      end in
    let ve = List.fold_right 
               (fun (arg,ty) ve' -> 
                  begin
                    typeOk ct tyVE ty;
                    VE.add arg ty ve'
                  end) params thisVE in
    let k = CT.find tycon ct in
    let super = Class.super k in
    let _ = overrideOk ct tyVE (name,super, Arrow(argTyps,retTyp)) in
    let (stmt',_) = elabStmt ct (tyVE,ve) (Method.body meth) in
    let realRetTyp = typeOfStmt stmt' in
    let _ = subtypeOk ct tyVE (realRetTyp,retTyp) in
      Method.make 
        ~name: name ~params: params ~body: stmt' ~ret_type: retTyp

  let elabCtor ct (tyVE,thisVE) ctor =
    let tycon = Con.tycon ctor in
    let k = (try CT.find tycon ct with
      | Not_found -> failwith @@ "Unknown tycon: "
                            ^(Tycon.toString tycon)^"\n") in
    let _ = assrt (Tycon.equal (tycon,Class.tycon k),
              "Invalid constructor name in class "
                   ^(Tycon.toString @@ Class.tycon k)) in
    let params = Con.params ctor in
    let ve = List.fold_right 
               (fun (arg,ty) ve' -> 
                  begin
                    typeOk ct tyVE ty;
                    VE.add arg ty ve'
                  end) params thisVE in
    let (stmt',_) = elabStmt ct (tyVE,ve) (Con.body ctor) in
      Con.make
        ~tycon: tycon ~params: params ~body: stmt'



  let bootStrapVE  ct ve tycon =
    let k = CT.find tycon ct in
    let tyvars = Class.tyvars k |> 
                   List.map (fun (tyvar,bound) -> tyvar) in
    let tyargs = List.map Type.var tyvars in
    let thisTyp = Type.mkApp (tycon,tyargs) in
    let thisVar = Var.fromString "this" in
      VE.add thisVar thisTyp ve

  let bootStrapTyVE ct tyVE tycon = 
    let k = CT.find tycon ct in
    let tyvardecs = Class.tyvars k in
      L.fold_right (fun (tyvar,typ) tyVE' -> 
                      TyVE.add tyvar typ tyVE') tyvardecs tyVE

  let elabClass ct k =
    (* k as `class` tycon<tyvardecs> `extends` super 
     *       {
     *          fieldDecs; ctors; meths
     *       }
     *)
    let tycon = Class.tycon k in
    let tyvarDecs = Class.tyvars k in
    let tyvarBounds = List.map snd tyvarDecs in
    let super = Class.super k in
    let fieldDecs = Class.fields k in
    let fieldTyps = List.map snd fieldDecs in
    let ctors = Class.ctors k in
    let meths = Class.methods k in
    (*
     * Initialize Γ and Δ.
     *)
    let emptyVE = VE.empty in
    let thisVE = bootStrapVE ct emptyVE tycon in
    let emptyTyVE = TyVE.empty in
    let tyVE = bootStrapTyVE ct emptyTyVE tycon in
    (*
     * Check well-formedness of types
     *)
    let _ =
      begin
        List.iter (typeOk ct tyVE) tyvarBounds;
        typeOk ct tyVE super;
        List.iter (typeOk ct tyVE) fieldTyps;
      end in
    (*
     * Check well-formedness, and also elaborate constructors and 
     * methods.
     *)
    let ctors' = List.map (elabCtor ct (tyVE,thisVE)) ctors in
    let meths' = List.map (fun meth -> 
                             elabMethod ct (tyVE,thisVE) meth tycon) 
                   meths in
      Class.make
        ~tycon: tycon
        ~tyvars: tyvarDecs
        ~super: super
        ~fields: fieldDecs
        ~ctors: ctors'
        ~methods: meths'

  let elabClassTable ct =
    CT.fold (fun tycon k ct' -> 
               if Tycon.isObject tycon then CT.add tycon k ct'
               else let k' = elabClass ct k in
                        CT.add tycon k' ct') ct CT.empty

      (*
  let doIt klasses =
    let ct = makeClassTable klasses in
      elabClassTable ct
       *)

  let doIt = elabClassTable
end 
