module type RTI_STRUCTS = 
sig
  module CT : Map.S with type key = Ast.Tycon.t
end
module Make (S:RTI_STRUCTS) =
struct
  open S
  open Utils
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
  module VE = Map.Make (struct
                          type t = Var.t
                          let compare = compare
                        end)
  module TyVE = Map.Make (struct
                          type t = Tyvar.t
                          let compare = compare
                        end)
  let ct = ref CT.empty;;
  let (++) s1 s2 = RVSet.union s1 s2
  let (--) s1 s2 = RVSet.diff s1 s2

  let object_class_tycon = Tycon.make @@ Id.fromString "Object"
  let object_class = Class.make 
    ~tycon: object_class_tycon ~rhoAlloc: (Rho.fresh()) 
    ~rhoBar: [] ~phi: Phi.truee ~tyvars: [] ~super: Type.Unknown 
    ~fields: [] ~ctors: [] ~methods: []

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
                 (fun rho -> Phi.outlives (rho,rhoAlloc)) rhoBar in
    let phi = CS.abduce (ctxt,residuePhi) in 
      Class.make
        ~tycon: tycon ~rhoAlloc: rhoAlloc' ~rhoBar: rhoBar'
        ~phi: phi ~tyvars: (List.combine tyvars tyvarBounds')
        ~super: super' ~fields: (List.combine fields fieldTyps')
        ~ctors: [] ~methods: []

  let bootStrapTyVE ct tyVE tycon = 
    let k = CT.find tycon ct in
    let tyvardecs = Class.tyvars k in
      L.fold_right (fun (tyvar,typ) tyVE' -> 
                      TyVE.add tyvar typ tyVE') tyvardecs tyVE

  let elabClass (ct':RegionAst.Class.t CT.t) (k:Ast.Class.t) =
    let hdB = elaborateHeader ct' k in
      hdB

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
