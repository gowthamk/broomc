(*
 * Utility functions
 *)

let assrt (cond,msg) = if cond then () else failwith msg
let rec printSV sep = function
  | [] -> ""
  | [str] -> str
  | str::strs -> str^sep^(printSV sep strs)

let printCSV = printSV ","

let mkUidGen idBase =
  let count = ref 0 in
    fun () -> 
      let id = idBase ^ (string_of_int !count) in
      let _ = count := !count + 1 in
        id
let mkSubstFn dom codom = 
  let substs = List.combine dom codom in
    fun a -> 
      try List.assoc a substs with
        | Not_found -> failwith @@ "Incomplete Substitution"
       

let rec tabulate n f l = if n<0 then l else
  tabulate (n-1) f @@ (f n)::l

module type STRINGABLE = 
sig
  type t
  val toString : t -> string
end 

module List = 
struct
  include List
  let tabulate n f = tabulate (n-1) f []
  let filterNotEq x l = List.filter (fun x' -> not (x' = x)) l
  let existsEq x l = List.exists (fun x' -> x' = x) l
end
