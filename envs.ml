module TyVE = Map.Make (struct
                        type t = Ast.Tyvar.t
                        let compare = compare
                      end)
module VE = Map.Make (struct
                        type t = Ast.Var.t
                        let compare = compare
                      end)
