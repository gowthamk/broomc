(*
 * Utility functions
 *)

let rec printCSV = function
  | [] -> ""
  | [str] -> str
  | str::strs -> str^","^(printCSV strs)

let mkUidGen idBase =
  let count = ref 0 in
    fun () -> 
      let id = idBase ^ (string_of_int !count) in
      let _ = count := !count + 1 in
        id

module type STRINGABLE = 
sig
  type t
  val toString : t -> string
end 
