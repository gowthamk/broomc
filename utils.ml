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
       
let andOf = List.fold_left (&&) true 

let rec tabulate n f l = if n<0 then l else
  tabulate (n-1) f @@ (f n)::l

let rec mapAndFold f b alist = match alist with
    [] -> ([],b)
  | x::xs -> 
      let (x',b') = f x b in
      let (xs',b'') = mapAndFold f b' xs in
        (x'::xs',b'')

let erongi f = (fun _ -> f ())

let curry f = (fun x y -> f (x,y))

module type STRINGABLE = 
sig
  type t
  val toString : t -> string
end 

module type PRINTABLE = 
sig
  type t
  val  print : t -> unit
end

module List = 
struct
  include List
  let tabulate n f = tabulate (n-1) f []
  let filterNotEq x l = List.filter (fun x' -> not (x' = x)) l
  let existsEq x l = List.exists (fun x' -> x' = x) l
  let mapAndFold = mapAndFold
  let snoc l x = List.append l [x]
end
