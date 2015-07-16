(*
 * Utility functions
 *)

let rec printCSV = function
  | [] -> ""
  | [str] -> str
  | str::strs -> str^","^(printCSV strs)

