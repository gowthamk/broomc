module type SYMBOL =
sig
  type t
  val equal : t*t -> bool
  val toString : t -> string
end
module type CONSTRAINT = 
sig
  type symbol_t
  type t =  True 
          | False
          | Outlives of symbol_t * symbol_t 
          | Eq of symbol_t * symbol_t 
          | NotEq of symbol_t * symbol_t
          | Conj of t list
          | Disj of t list
end
module Make (S: sig
                  module Symbol : SYMBOL
                  module Constraint : CONSTRAINT 
                    with type symbol_t = Symbol.t
                end) =
struct
  open S
  module Phi = Constraint
  module PhiSet = Set.Make (struct
                              type t = Phi.t
                              let compare = compare
                            end)

  type sol_t = {substFn: Symbol.t -> Symbol.t;
                residue: Phi.t;}

  let elimCommonSubExp phi =
    let open Phi in
    let elimDupes props = PhiSet.elements @@ PhiSet.of_list props in
      match phi with
        | Conj props -> Conj (elimDupes props)
        | Disj props -> Disj (elimDupes props)
        | _ -> phi

  let normalize phi = 
    let phi' = elimCommonSubExp phi in
      {substFn = (fun x -> x);
       residue = phi'}
  let abduce (ctxt,phi) = phi
end
