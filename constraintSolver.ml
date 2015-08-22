module type SYMBOL =
sig
  type t
  val equal : t*t -> bool
  val toString : t -> string
  val isConcrete : t -> bool
  val compare : t -> t -> int
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
  module PhiGraph : 
  sig
    type t
    val create : unit -> t
    val add_vertex : t -> Symbol.t -> unit
    val remove_vertex : t -> Symbol.t -> unit
    val add_edge : t -> (Symbol.t*Symbol.t) -> unit
    val fold_edges : (Symbol.t*Symbol.t -> 'a -> 'a) -> t -> 'a -> 'a
    val scc_list : t -> Symbol.t list list 
    val output_graph : out_channel -> t -> unit
  end= 
  struct
    module Vertex = struct
      type t = Symbol.t
      let compare = Pervasives.compare
      let hash = Hashtbl.hash
      let equal = curry Symbol.equal
    end 
    module DiGraph = Graph.Imperative.Digraph.ConcreteBidirectional(Vertex)
    module SCC = Graph.Components.Make(DiGraph)
    module Dot = Graph.Graphviz.Dot
      (struct
         include DiGraph
         let edge_attributes _ = []
         let default_edge_attributes _ = []
         let get_subgraph _ = None
         let vertex_attributes _ = []
         let vertex_name = Symbol.toString
         let default_vertex_attributes _ = []
         let graph_attributes _ = []
       end)
    include SCC
    include DiGraph
    include Dot
    let create () = create ()
    let add_edge t (v1,v2) = add_edge t v1 v2
    let fold_edges f = fold_edges (fun v1 v2 acc -> f (v1,v2) acc)
  end

  type sol_t = {fnM: Symbol.t -> Symbol.t;
                fnF: Symbol.t -> Symbol.t;
                residue: Phi.t;}

  let genDotFileName = 
    let f = mkUidGen "Graph" in
      fun s -> (f ())^"_"^s^".dot"

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

  let mkGraph cs = 
    let open PhiGraph in
    let g = create () in
    let addVertex = add_vertex g in
    let addEdge = add_edge g in
    let addToGraph = function
      | Phi.Eq (s1,s2) -> 
          begin
            addVertex s1;
            addVertex s2;
            addEdge (s1,s2);
            addEdge (s2,s1);
          end
      | Phi.Outlives (s1,s2) ->
          begin
            addVertex s1;
            addVertex s2;
            addEdge (s1,s2);
          end
      | Phi.True -> () | Phi.False -> failwith "Unsat Constraints" 
      | _ -> failwith "Unimpl." in
    let _ = List.iter addToGraph cs in 
      g

  let simplify cs = 
    let open PhiGraph in 
    let mkSccRepMap scc_syms = 
      let scc_syms = List.sort (Symbol.compare) scc_syms in
      let rep = List.hd scc_syms in 
        List.map (fun s -> (s,rep)) scc_syms in
    let mkF sccs = mkFn2 @@ List.concat @@
                   List.map mkSccRepMap sccs in
    let g = mkGraph cs in 
    let  _ = output_graph (open_out_bin @@ genDotFileName "cs") g in
    let fnF = mkF (scc_list g) in
    let cs' = List.map (Phi.mapSymbols fnF) cs in 
    let g_res = mkGraph cs' in
    let  _ = output_graph (open_out_bin @@ genDotFileName "res") g_res in
    let cs_res = fold_edges 
       (fun (s1,s2) acc -> if Symbol.equal (s1,s2) then acc
                  else (Phi.Outlives (s1,s2))::acc) g_res [] in
    let phi_res = Phi.Conj cs_res in
      (fnF,phi_res)

  (*
   * Given phi_cx and phi_cs, returns a 
   *)
  let normalize (phi_cx,phi_cs) = 
    let phi = Phi.conj [phi_cx;phi_cs] in
    let cs = match phi with 
      | Phi.Conj cs -> cs |  _ -> [phi] in
    let (m,cs') = simpleSolve [] cs in
    let fnM = mkPartialFn2 m in
    let psiCs = Phi.mapSymbols 
                  (fun s -> if Symbol.isConcrete s then s
                            else fnM s) in 
    (*
     * Although fnM is a partial fn, but it shoudl be complete
     * over the domain of non-concrete symbols. It is an error
     * if it is not so. The following will see to that.
     *)
    let (fnF,phi_res) = simplify @@ List.map psiCs cs' in 
      {fnM = (fun rho -> fnF @@ fnM rho); 
       fnF = fnF; residue = phi_res}

  let abduce (ctxt,phi) = elimCommonSubExp phi
end
