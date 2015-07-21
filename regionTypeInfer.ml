module type RTI_STRUCTS = 
sig
  module CT : Map.S with type key = Ast.Tycon.t
end
module Make (S:RTI_STRUCTS) =
struct
  open S
  open Utils
  open Envs
  open Ast
  open RegionAst
  module A = Ast
  module ATy = A.Type
  module RA = RegionAst
  module L = List
  module Rho = RegionVar
  module R = RegionVar
  module RVSet = RegionVarSet
  module Phi = RegionConstraint
  module CS = ConstraintSolve
  module MN = MethodName
  module M = Method
  module FGJ = TypeCheck.Make (struct
                                 module CT = CT
                               end)

  let ct = ref CT.empty;;

  let (++) s1 s2 = RVSet.union s1 s2
  let (--) s1 s2 = RVSet.diff s1 s2

  type arrow_t = {rhoAlloc : Rho.t; 
                  rhoBar : Rho.t list; 
                  phi : Phi.t;
                  argTys : Type.t list; 
                  retTy: Type.t}

  let object_class_tycon = Tycon.make @@ Id.fromString "Object"
  let object_class = Class.make 
    ~tycon: object_class_tycon ~rhoAlloc: (Rho.fresh()) 
    ~rhoBar: [] ~phi: Phi.truee ~tyvars: [] ~super: Type.Unknown 
    ~fields: [] ~ctors: [] ~methods: []

  let isTopTyp = function 
    | Type.Object _ -> true
    | Type.ConApp _ -> false
    | typ -> failwith @@ "isTopTyp: "^(Type.toString typ)
              ^" is not a class type.\n"
  (*
   * Unifies (a) Region args to tycon with formal region vars, and 
   * (b) tyargs to tycon with formal tyvars, and generates two 
   * substitution functions. 
   * Convention requires that first region arg be the inAllocCtxt.
   *)
  let unifyTyconArgs ct' tycon rargs tyargs = 
    let invalidConAppMsg = "Invalid instantiation of region-\
           \ parametric class "^(Tycon.toString tycon)^"\n" in
    let k = (try CT.find tycon ct' with
      | Not_found -> failwith @@ "Unknown tycon: "
                            ^(Tycon.toString tycon)^"\n") in
    let rhoA = Class.rhoAlloc k in
    let rhoBar = Class.rhoBar k in
    let rhos = rhoA :: rhoBar in
    let _ = 
      begin
        assrt (List.length rhos = List.length rargs,
               invalidConAppMsg);
      end in
    let rhoSubstFn = mkSubstFn rhos rargs in
    let tyvars = List.map fst (Class.tyvars k) in
    let tyvarSubstFn = mkSubstFn tyvars tyargs in
      (rhoSubstFn, tyvarSubstFn)

  let rec templateTy ct' = let open Type in function
    | ATy.Int -> Int | ATy.Bool -> Bool
    | ATy.Unit -> Unit | ATy.Unknown -> Unknown
    | ATy.Any -> Any | ATy.Tyvar v -> Tyvar v
    | ATy.Object -> Object (Rho.fresh())
    | ATy.ConApp (tycon,tyargs) -> 
        if A.Tycon.isRegion tycon then 
          let rootTy = match tyargs with [x] -> x
            | _ -> failwith "Invalid region class type" in
          let rootTyX = templateTy ct' rootTy in
          let thisRgnRho = Rho.fresh () in
          let substFn = fun rho -> thisRgnRho in
          let rootTy' = Type.mapRegionVars substFn rootTyX in
          let rhoAlloc = Rho.fresh () in
          let thisRgnTy = Type.Region {rho=thisRgnRho;
                                   rhoAlloc=rhoAlloc;
                                   rootObjTy = rootTy';} in
            Type.Exists (thisRgnRho,thisRgnTy)
        else
          try
            let k = CT.find tycon ct' in
            let nRgnParams = List.length (Class.rhoBar k) + 1 
              (* the +1 is for rhoAlloc *) in
            let rargs = List.tabulate nRgnParams @@ 
                          fun _ -> Rho.fresh() in
            let tyargs' = List.map (templateTy ct') tyargs in
              ConApp {tycon=tycon; rAlloc = List.hd rargs;
                      rBar = List.tl rargs; tyArgs = tyargs';}
          with | Not_found -> 
            let tyargs' = List.map (templateTy ct') tyargs in
              ConApp {tycon=tycon; rAlloc = Rho.dummy;
                      rBar = []; tyArgs = tyargs';}

  let allocRgn = let open Type in function
    | ConApp t -> t.rAlloc
    | Object rhoA -> rhoA
    | Region t -> t.rhoAlloc
    | t -> failwith @@ "Allocation region of "
                    ^(Type.toString t)^" unknown"

  let rec typeOk ct' (allRhos,liveRhos) ty = 
    let open Type in
    let type_ok = typeOk ct' (allRhos,liveRhos) in
      match ty with
        | Object rho -> Phi.inSet (rho,liveRhos)
        | Region args ->
            let c1 = Phi.inSet (args.rho,allRhos) in
            let c2 = Phi.inSet (args.rhoAlloc,liveRhos) in
            let c3 = Phi.subEq (RVSet.of_list @@ frv(args.rootObjTy),
                                RVSet.singleton args.rho) in
              Phi.conj [c1; c2; c3]
        | ConApp args ->
            let c1 = List.map type_ok args.tyArgs in
            let c2 = Phi.inSet (args.rAlloc, liveRhos) in
            let c3 = Phi.subEq (RVSet.of_list args.rBar, liveRhos) in
            let k = CT.find (args.tycon) ct' in
            let (rhoA,rhoBar) = (Class.rhoAlloc k,Class.rhoBar k) in
            let (rA,rBar) = (args.rAlloc,args.rBar) in
            let rhoSubstFn = mkSubstFn (rhoA::rhoBar) (rA::rBar) in
            let phi = Class.phi k in
            let c4 = Phi.mapRegionVars rhoSubstFn phi in
            let c5 = List.map (fun r -> Phi.outlives (r,rA)) rBar in
              Phi.conj @@ List.concat [c1; [c2; c3; c4;]; c5]
        | _ -> Phi.truee

  let superOfClassTy ct' classTyp = 
    let (tycon,rargs,tyargs) = match classTyp with 
      | Type.ConApp {Type.tycon; rAlloc; rBar; tyArgs} -> 
            (tycon, rAlloc::rBar, tyArgs) 
      | _ -> exit 1 in
    let (rhoSubstFn,alphaSubstFn)= unifyTyconArgs ct' 
                                     tycon rargs tyargs in
    let k = CT.find tycon ct' in 
    let formalSuper = Class.super k in
    let actualSuper = Type.mapRegionVars rhoSubstFn formalSuper |>
                      Type.mapTyvars alphaSubstFn in
      actualSuper
    

  let rec subtypeOk ct' (liveRhos,tyVE) (subTy,supTy) =
    let open Type in
    let subtype_ok = subtypeOk ct' (liveRhos,tyVE) in
      match (subTy,supTy) with
        | (Tyvar tyv,_) -> subtype_ok (TyVE.find tyv tyVE,supTy)
        | (ConApp args1,ConApp args2) -> 
            if Tycon.equal (args1.tycon, args2.tycon) then
              Type.equal (subTy,supTy)
            else
              let superOfSubTy = superOfClassTy ct' subTy in
                subtype_ok (superOfSubTy,supTy)
        | _ -> Type.equal (subTy,supTy)

  (*
   * What is the type of method classTyp.mname ?
   *)
  exception Return of arrow_t
  let rec mtype ct' (classTyp,mname) = 
    try
      let _ = if isTopTyp classTyp then raise Not_found else () in
      let (tycon,rargs,tyargs) = match classTyp with 
        | Type.ConApp {Type.tycon; rAlloc; rBar; tyArgs} -> 
            (tycon, rAlloc::rBar, tyArgs) 
        | _ -> exit 1 in
      let (psiRho, psiAlpha) = 
        unifyTyconArgs ct' tycon rargs tyargs in
      let psiRhoInTy = Type.mapRegionVars psiRho in
      let psiAlphaInTy = Type.mapTyvars psiAlpha in
      let k = CT.find tycon ct' in 
      let formalSuper = Class.super k in
      let actualSuper = Type.mapRegionVars psiRho formalSuper |> 
                        Type.mapTyvars psiAlpha in
      let meths = Class.methods k in
      let m = 
        try 
          List.find (fun m -> MN.equal (M.name m,mname)) meths 
        with Not_found -> 
          raise @@ Return (mtype ct' (actualSuper,mname)) in 
      let actualPhi = Phi.mapRegionVars psiRho (M.phi m) in
      let actualArgTys = 
          List.map (fun (_,ty) -> psiAlphaInTy @@ psiRhoInTy ty) 
                         (M.params m) in
      let actualRetTy = psiAlphaInTy @@ psiRhoInTy (M.ret_type m) in
          {rhoAlloc = M.rhoAlloc m; 
           rhoBar = M.rhoBar m; 
           phi = actualPhi;
           argTys = actualArgTys; 
           retTy = actualRetTy}
    with | Return x -> x

  let rec ftype ct' (classTyp,f) =
    let _ = if isTopTyp classTyp then raise Not_found else () in
    let (tycon,rargs,tyargs) = match classTyp with 
      | Type.ConApp {Type.tycon; rAlloc; rBar; tyArgs} -> 
          (tycon, rAlloc::rBar, tyArgs) 
      | _ -> exit 1 in
    let (psiRho, psiAlpha) = unifyTyconArgs ct' tycon rargs tyargs in
    let k = CT.find tycon ct' in 
    let formalSuper = Class.super k in
    let actualSuper = Type.mapRegionVars psiRho formalSuper |> 
                      Type.mapTyvars psiAlpha in
    let fieldDecs = Class.fields k in
      try 
        List.find (fun (f',ty) -> Field.equal (f,f')) 
          fieldDecs |> snd |> Type.mapRegionVars psiRho |> 
                              Type.mapTyvars psiAlpha
      with Not_found -> ftype ct' (actualSuper,f)

  let typeOfVar ve v = try VE.find v ve with
    | Not_found -> failwith @@ "Region type for var "
                          ^(Var.toString v)^" not found"

  type elab_ctxt_t = {allRhos : RVSet.t; 
                      liveRhos : RVSet.t; 
                      tyVE : Type.t TyVE.t;
                      ve : Type.t VE.t;
                      rhoAlloc : Rho.t}

  let elabPackExpr ct' ctxt exp (subTy,supTy) = 
    let open Type in
    let subtype_ok = subtypeOk ct' (ctxt.liveRhos,ctxt.tyVE) in
      match (subTy,supTy) with
        | (Region {rho}, Exists (boundRho,t)) -> 
            let substFn = fun rho' -> 
              if Rho.equal (rho',boundRho) then rho else rho' in
            let t' = Type.mapRegionVars substFn t in
            let c = subtype_ok (subTy,t') in
            let exp' = Expr.make (Expr.Pack (rho,exp),supTy) in
              (exp',c)
        | _ -> (exp, subtype_ok (subTy,supTy))

  let rec elabExpr ct' ctxt (exp:A.Expr.t) = 
    let open Expr in 
    let (allRhos,liveRhos) = (ctxt.allRhos, ctxt.liveRhos) in
    let (tyVE, ve, rAlloc) = (ctxt.tyVE, ctxt.ve, ctxt.rhoAlloc) in
    let templateTy = templateTy ct' in
    let type_ok = typeOk ct' (allRhos,liveRhos) in
    let rgn_type_of_var = typeOfVar ve in
    let (node,ty) = (A.Expr.node exp, A.Expr.typ exp) in
    let doIt = elabExpr ct' ctxt in
    let maybePackExpr e ty = 
      elabPackExpr ct' ctxt e (Expr.typ e, ty) in
    let ret e = (e,Phi.truee) in
      match node with 
        | A.Expr.Int i -> ret @@ make (Int i,Type.Int) 
        | A.Expr.Bool b -> ret @@ make (Bool b,Type.Bool) 
        | A.Expr.Var v -> ret @@ make (Var v,rgn_type_of_var v)
        | A.Expr.FieldGet (e,f) -> 
            let (e',c1) = elabExpr ct' ctxt e in
            let classTyp = typ e' in
            let fRgnTy = ftype ct' (classTyp,f) in
              (make (FieldGet (e',f),fRgnTy), c1)
        | A.Expr.MethodCall (objExp,mn,argExps) ->
            let (objExp',c1) = doIt objExp in
            let (argExps',c2s) = List.split @@ 
                                 List.map doIt argExps in
            let classTyp = Expr.typ objExp' in
            let {rhoAlloc; rhoBar; phi; argTys=formalArgTys; 
                 retTy=formalRetTy} = mtype ct' (classTyp,mn) in
            let rBar = List.map (erongi Rho.fresh) rhoBar in
            let c3 = Phi.subEq (RVSet.of_list rBar, liveRhos) in
            (* rAlloc is the current inAllocCtxt passed via ctxt *)
            let psiRho = mkSubstFn (rhoAlloc::rhoBar) 
                                   (rAlloc::rBar) in
            let c4 = Phi.mapRegionVars psiRho phi in
            let psiRhoInTy = Type.mapRegionVars psiRho in
            let actualArgTys = List.map psiRhoInTy formalArgTys in
            let actualRetTy = psiRhoInTy formalRetTy in
            (* argExpTys must be subtype of actualArgTys *)
            let (argExps'',c5s) = List.split @@ 
                    List.map2 maybePackExpr argExps' actualArgTys in 
            let node' = Expr.MethodCall {Expr.meth = (objExp',mn);
                                   rAlloc = rAlloc; rBar = rBar; 
                                   args = argExps''} in
            let c = Phi.conj @@List.append (c1::c2s) (c3::c4::c5s) in
              (Expr.make (node',actualRetTy), c)
        | A.Expr.New (tycon,tyargs,argExps) -> 
            let classTyp' = templateTy @@ 
                            A.Type.ConApp (tycon,tyargs) in
            let c1 = type_ok classTyp' in
            let rhoAlloc = allocRgn classTyp' in
            let (argExps',c2s) = List.split @@ List.map 
                                                 doIt argExps in
            let argExpTys' = List.map Expr.typ argExps' in
            let k = CT.find tycon ct' in
            (* We pick applicable ctor based on Java semantics *)
            let applicableCtor ctor = 
              let paramTys' = List.map snd (Con.params ctor) in
                if List.length paramTys' = List.length argExpTys' 
                then
                  let paramTys = List.map Type.erase paramTys' in
                  let argExpTys = List.map Type.erase argExpTys' in
                      List.for_all2 
                        (fun argExpTy paramTy -> 
                            FGJ.isSubtype (!ct) (TyVE.map Type.erase tyVE) 
                              (argExpTy,paramTy)) 
                        argExpTys paramTys
                else false in
            let ctor = List.find applicableCtor @@ Class.ctors k in
            let argTys' = List.map snd @@ Con.params ctor in
            let (argExps'',c3s) = List.split @@ 
                        List.map2 maybePackExpr argExps' argTys' in
            (* Currently, we only allow objects to be allocated in
             * inAllocCtxt *)
            let cOutlives = Phi.equal(rhoAlloc,rAlloc) in
            let node' = Expr.New (classTyp',argExps'') in
            let c = Phi.conj @@ List.concat 
                                  [[c1]; c2s; c3s; [cOutlives]] in
              (Expr.make (node',classTyp'),c)
        | _ -> failwith "Unimpl."

  let rec elabStmt ct' ctxt stmt = 
    let open Stmt in
    let (allRhos,liveRhos) = (ctxt.allRhos, ctxt.liveRhos) in
    let (tyVE, ve, rhoAlloc) = (ctxt.tyVE, ctxt.ve, ctxt.rhoAlloc) in
    let templateTy = templateTy ct' in
    let type_ok = typeOk ct' (allRhos,liveRhos) in
    let subtype_ok = subtypeOk ct' (liveRhos,tyVE) in
    let rgn_type_of_var = typeOfVar ve in
      match stmt with
        | A.Stmt.VarDec (ty,v,e) -> 
            let ty' = templateTy ty in
            let c1 = type_ok ty' in
            let (e',c2) = elabExpr ct' ctxt e in
            let e'ty = Expr.typ e' in
            let c3 = subtype_ok (e'ty,ty') in
            let c = Phi.conj [c1; c2; c3] in
            let extendedVE = VE.add v ty' ve in
            let stmt' = VarDec (ty',v,e') in
              (stmt',{ctxt with ve = extendedVE},c)
        | A.Stmt.Assn (v,e) ->
            let vTy = rgn_type_of_var v in
            let (e',c1) = elabExpr ct' ctxt e in
            let e'ty = Expr.typ e' in
            let c2 = subtype_ok (e'ty,vTy) in
            let c = Phi.conj [c1; c2] in
            let stmt' = Assn (v,e') in
              (stmt',ctxt,c)
        | A.Stmt.FieldSet (e1,e2) ->
            let (e1',c1) = elabExpr ct' ctxt e1 in
            let (e2',c2) = elabExpr ct' ctxt e2 in
            let (e1'ty,e2'ty) = (Expr.typ e1', Expr.typ e2') in
            let (e2'',c3) = elabPackExpr ct' ctxt e2' (e2'ty,e1'ty) in
            let c = Phi.conj [c1; c2; c3] in
            let stmt' = Stmt.FieldSet (e1',e2') in
              (stmt',ctxt,c)
        | A.Stmt.Expr e -> 
            let (e',c) = elabExpr ct' ctxt e in
            let stmt' = Stmt.Expr e' in
              (stmt',ctxt,c)
        | A.Stmt.Seq stmts -> 
            let (stmts',(ctxt',clist)) = 
              List.mapAndFold 
                (fun stmt (ctxt,clist) ->
                   let (stmt',ctxt',cnew) = elabStmt ct' ctxt stmt in
                     (stmt', (ctxt', cnew::clist))) 
                (ctxt,[]) stmts in
            let stmt' = Stmt.Seq stmts' in
            (* clist has constraints in reverse order *)
            let c = Phi.conj @@ List.rev clist in
              (stmt',ctxt',c)
        | _ -> failwith "Unimpl."

  let headerTemplate ct' k = 
    (* Original header *)
    let tycon = A.Class.tycon k in
    let (tyvars,tyvarBounds) = List.split (A.Class.tyvars k) in
    let super = A.Class.super k in
    let (fields,fieldTyps) = List.split (A.Class.fields k) in
    (* Templates *)
    let tyvarBoundsX = List.map (templateTy ct') tyvarBounds in
    let superX = templateTy ct' super in
    let fieldTypsX = List.map (templateTy ct') fieldTyps in
    let rhoAlloc = allocRgn superX in
    let frv = Type.frv in
    let freeRhos = List.concat 
                     [List.concat @@ List.map frv tyvarBoundsX;
                      frv superX;
                      List.concat @@ List.map frv fieldTypsX] in
    let rhoBar = List.filterNotEq rhoAlloc freeRhos in
    let newTy = Type.ConApp {Type.tycon = tycon; rAlloc = rhoAlloc;
                             rBar = rhoBar; 
                             tyArgs = List.map Type.var tyvars} in
    let psiI = function
      | Type.ConApp {Type.tycon=tycon'} -> if Tycon.equal (tycon',tycon)
                                        then Some newTy else None
      | _ -> None in
    let applyPsiI = Type.map psiI in
    let tyvarBounds' = List.map applyPsiI tyvarBoundsX in
    let super' = applyPsiI superX in
    let fieldTyps' = List.map applyPsiI fieldTypsX in
      Class.make
        ~tycon: tycon ~rhoAlloc: rhoAlloc ~rhoBar: rhoBar
        ~phi: Phi.truee ~tyvars: (List.combine tyvars tyvarBounds')
        ~super: super' ~fields: (List.combine fields fieldTyps')
        ~ctors: [] ~methods: []

  let elaborateHeader ct' k = 
    let hdK= headerTemplate ct' k in
    let _ =
        begin
          print_string "Header template:\n";
          Class.print hdK;
          Format.printf "@\n";
        end in
    (* Recursive occurances of tycon are ok'ed under 
     * extended class table (ct'') *)
    let tycon = (A.Class.tycon k) in
    let ct'' = CT.add tycon hdK ct' in
    let rhoAlloc = Class.rhoAlloc hdK in
    let rhoBar = Class.rhoBar hdK in
    let (tyvars,tyvarBounds) = List.split (Class.tyvars hdK) in
    let super = Class.super hdK in
    let (fields,fieldTyps) = List.split (Class.fields hdK) in
    let allRhos = RVSet.of_list @@ rhoAlloc::rhoBar in
    let liveRhos = RVSet.of_list @@ rhoAlloc::rhoBar in
    let type_ok = typeOk ct'' (allRhos, liveRhos) in
    let c1 = Phi.conj @@ List.map type_ok tyvarBounds in
    let c2 = type_ok super in
    let c3 = Phi.conj @@ List.map type_ok fieldTyps in
    let cOutlives = Phi.conj @@ List.map 
                  (fun rho -> Phi.outlives (rho,rhoAlloc)) rhoBar in
    let c = Phi.conj [c1; c2; c3; cOutlives] in
    let {CS.substFn; residue=residuePhi} = CS.normalize c in
    let psiI = Type.mapRegionVars substFn in
    let tyvarBounds' = List.map psiI tyvarBounds in
    let super' = psiI super in
    let fieldTyps' = List.map psiI fieldTyps in
    let rhoAlloc' = substFn rhoAlloc in
    let rhoBar' = let open Type in RVSet.elements @@
      (RVSet.of_list @@ List.concat [frvStar tyvarBounds'; 
                                     frv super'; 
                                     frvStar fieldTyps'])
      -- (RVSet.singleton rhoAlloc') in
    let ctxt = Phi.conj @@ List.map 
                 (fun rho -> Phi.outlives (rho,rhoAlloc')) rhoBar' in
    let phi = CS.abduce (ctxt,residuePhi) in 
      Class.make
        ~tycon: tycon ~rhoAlloc: rhoAlloc' ~rhoBar: rhoBar'
        ~phi: phi ~tyvars: (List.combine tyvars tyvarBounds')
        ~super: super' ~fields: (List.combine fields fieldTyps')
        ~ctors: [] ~methods: []

  let bootStrapTyVE ct' tyVE tycon = 
    let k = CT.find tycon ct' in
    let tyvardecs = Class.tyvars k in
      L.fold_right (fun (tyvar,typ) tyVE' -> 
                      TyVE.add tyvar typ tyVE') tyvardecs tyVE

  let bootStrapVE  ct' ve tycon =
    let k = CT.find tycon ct' in
    let tyvars = Class.tyvars k |> 
                   List.map (fun (tyvar,bound) -> tyvar) in
    let tyargs = List.map A.Type.var tyvars in
    let thisTyp = A.Type.mkApp (tycon,tyargs) in
    let thisRgnTyp = templateTy ct' thisTyp in
    let thisVar = Var.fromString "this" in
      VE.add thisVar thisRgnTyp ve

  let elabCtor ct' (ctor:A.Con.t) hdK = 
    let rhoAlloc = Class.rhoAlloc hdK in
    let rhoBar = Class.rhoBar hdK in
    let phi = Class.phi hdK in
    let (tyvars,tyvarBounds) = List.split (Class.tyvars hdK) in
    let super = Class.super hdK in
    let (fields,fieldTyps) = List.split (Class.fields hdK) in
    let allRhos = RVSet.of_list @@ rhoAlloc::rhoBar in
    let liveRhos = RVSet.of_list @@ rhoAlloc::rhoBar in
    (* Recursive occurances of tycon need
     * extended class table (ct'') *)
    let tycon = (Class.tycon hdK) in
    let ct'' = CT.add tycon hdK ct' in
    let templateTy = templateTy ct'' in
    let type_ok = typeOk ct'' (allRhos, liveRhos) in
    let (params,paramTys) = List.split @@ A.Con.params ctor in
    let paramTysX = List.map templateTy paramTys in
    let c1 = Phi.conj @@ List.map type_ok paramTysX in
    let thisVE = bootStrapVE ct'' (VE.empty) tycon in
    let extendedVE = List.fold_right2 VE.add 
                       params paramTysX thisVE in
    let tyVE = bootStrapTyVE ct'' (TyVE.empty) tycon in
    let (stmtX,_,c2) = 
    (*
     * inAllocCtxt for ctor body is same as that of the class.
     *)
      elabStmt ct' {allRhos = allRhos; liveRhos = liveRhos; 
                    tyVE = tyVE; ve = extendedVE; 
                    rhoAlloc = rhoAlloc;} (A.Con.body ctor) in
    let cOutlives = List.map 
                (fun rho -> Phi.outlives (rho,rhoAlloc)) rhoBar in
    let c = Phi.conj @@ c1::c2::phi::cOutlives in
    let {CS.substFn;residue=residuePhi} = CS.normalize c in
    let psiI = Type.mapRegionVars substFn in
    let psiStmt = Stmt.mapRegionVars substFn in
    let tyvarBounds' = List.map psiI tyvarBounds in
    let super' = psiI super in
    let fieldTyps' = List.map psiI fieldTyps in
    let rhoAlloc' = substFn rhoAlloc in
    let rhoBar' = let open Type in RVSet.elements @@
      (RVSet.of_list @@ List.concat [frvStar tyvarBounds'; 
                                     frv super'; 
                                     frvStar fieldTyps'])
      -- (RVSet.singleton rhoAlloc') in
    let paramTys' = List.map psiI paramTysX in
    let paramRhos = RVSet.of_list @@ List.concat @@ 
                    List.map Type.frv paramTys' in
    let classRhos = RVSet.of_list @@ rhoAlloc'::rhoBar' in
    let _ = assrt (RVSet.subset paramRhos classRhos,
                   "A constructor cannot be region-polymorphic.") in
    let ctxt = Phi.conj @@ List.map 
               (fun rho -> Phi.outlives (rho,rhoAlloc')) rhoBar' in
    let phi' = CS.abduce (ctxt,residuePhi) in 
    let stmt' = psiStmt (stmtX) in
    let con' = Con.make ~tycon:tycon 
                        ~params:(List.combine params paramTys')
                        ~body:stmt' in
    let hdK' = Class.make
        ~tycon: tycon ~rhoAlloc: rhoAlloc' ~rhoBar: rhoBar'
        ~phi: phi' ~tyvars: (List.combine tyvars tyvarBounds')
        ~super: super' ~fields: (List.combine fields fieldTyps')
        ~ctors: [] ~methods: [] in
      (con',hdK')

  let elaborateCons ct' (k,hdK) = 
    let ctors = A.Class.ctors k in
    let (ctors',hdK') = List.mapAndFold (elabCtor ct') 
                          hdK ctors in

      {hdK' with Class.ctors=ctors'}

  let elabClass (ct':RegionAst.Class.t CT.t) (k:Ast.Class.t) =
    let hdB = elaborateHeader ct' k in
    let consB = elaborateCons ct' (k,hdB) in
      consB

  let elabClassTable (ct : Ast.Class.t CT.t) =
    CT.fold (fun tycon k ct' -> 
               if Tycon.isObject tycon 
               then CT.add object_class_tycon object_class ct'
               else let k' = elabClass ct' k in
                        CT.add tycon k' ct') ct CT.empty

  let doIt (classTab : Ast.Class.t CT.t) = 
    let _ = ct := classTab in
      elabClassTable classTab
end
