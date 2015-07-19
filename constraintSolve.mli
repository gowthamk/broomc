open RegionAst

type sol_t = {substFn: RegionVar.t -> RegionVar.t;
              residue: RegionConstraint.t;}

val normalize : RegionConstraint.t -> sol_t
val abduce : (RegionConstraint.t * RegionConstraint.t) 
            -> RegionConstraint.t
