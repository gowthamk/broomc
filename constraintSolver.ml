module type SYMBOL =
sig
  type t
  val equal : t*t -> bool
  val toString : t -> string
  val isConcrete : t -> bool
end
module type CONSTRAINT = 
sig
  type symbol_t
  type t =  True 
          | False
          | Outlives of symbol_t * symbol_t 
          | Eq of symbol_t * symbol_t 
          | NotEq of symbol_t * symbol_t
          | NotOutlives of symbol_t * symbol_t
          | Conj of t list
          | Disj of t list
  val conj : t list -> t
  val mapSymbols : (symbol_t -> symbol_t) -> t -> t
end
module Make (S: sig
                  module Symbol : SYMBOL
                  module Constraint : CONSTRAINT 
                    with type symbol_t = Symbol.t
                end) =
struct
  open Utils
  open S
  module Phi = Constraint
  module PhiSet = Set.Make (struct
                              type t = Phi.t
                              let compare = compare
                            end)

  type sol_t = {fnM: Symbol.t -> Symbol.t;
                residue: Phi.t;}

  let elimCommonSubExp phi =
    let open Phi in
    let elimDupes props = PhiSet.elements @@ PhiSet.of_list props in
      match phi with
        | Conj props -> Conj (elimDupes props)
        | Disj props -> Disj (elimDupes props)
        | _ -> phi

  let rec simpleSolve m cs = 
    let isConc = Symbol.isConcrete in
    let csToMaybeSol = function
      | Phi.Eq (rv1,rv2) -> if not (isConc rv1) && (isConc rv2)
        then Some (rv1,rv2)
        else if (isConc rv1) && not (isConc rv2) then Some (rv2,rv1)
        else None 
      | _ -> None in
      match List.mapSome csToMaybeSol cs with
        | [] -> (m,cs)
        | eqs -> 
            let m' = List.sort_uniq compare eqs in 
            let partialFnM = fun a -> try List.assoc a m'
                                with Not_found -> a in
            let psiCs = Phi.mapSymbols partialFnM in 
            let cs' = List.map psiCs cs in 
              simpleSolve (List.append m m') cs'

  (*
   * Given phi_cx and phi_cs, returns a 
   *)
  let normalize (phi_cx,phi_cs) = 
    let phi = Phi.conj [phi_cx;phi_cs] in
    let phi' = elimCommonSubExp phi in
    let cs = match phi' with 
      | Phi.Conj cs -> cs
      | Phi.Disj _ -> failwith "Unimpl." | _ -> [phi'] in
    let (m,cs') = simpleSolve [] cs in
    let fnM = mkSubstFn2 m in
    let psiCs = Phi.mapSymbols 
                  (fun s -> if Symbol.isConcrete s then s
                            else fnM s) in 
    (*
     * Unlike partialFnM, fnM should be a complete function 
     * over the domain of non-concrete symbols. It is an error
     * if it is not so. The following will see to that.
     *)
    let phi_res = Phi.conj @@ List.map psiCs cs' in 
      {fnM = fnM;
       residue = phi_res}
  let abduce (ctxt,phi) = phi
end
