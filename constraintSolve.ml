open RegionAst

type sol_t = {substFn: RegionVar.t -> RegionVar.t;
              residue: RegionConstraint.t;}

let normalize phi = {substFn = (fun x -> x);
                 residue = phi}
let abduce (ctxt,phi) = phi
