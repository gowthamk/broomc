{
  open Parser
  module P = Printf
  module L = Lexing

  let keyword_table : (string, token) Hashtbl.t = Hashtbl.create 32
  let _ = List.iter (fun (k, v) -> Hashtbl.add keyword_table k v)
    [("class"   , T_keyword_class)
    ;("extends" , T_keyword_extends)
    ;("new"     , T_keyword_new)
    ;("super"   , T_keyword_super)
    ;("return"  , T_keyword_return)
    ;("void"     , T_keyword_void)
    ;("int"     , T_keyword_int)
    ;("bool"    , T_keyword_bool)]

  let error msg lexbuf =
    failwith (P.sprintf "error: %s %d-%d\n" msg
             (L.lexeme_start lexbuf)
             (L.lexeme_end lexbuf))

  let warning msg lexbuf =
    P.eprintf "warning: %s %d-%d\n" msg
             (L.lexeme_start lexbuf)
             (L.lexeme_end lexbuf)
}

let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']

let letter = lower | upper

let newline = ['\n' '\r']
let whitespace = [' ' '\n' '\r' '\t']

let op_symbol = ['+' '-' '*' '/' '%' '=' '!' '?' '<' '>' '|' '&' '^' '@']
let op = op_symbol+

rule token = parse
| whitespace+       { token lexbuf }
| "/*"              { comment lexbuf; token lexbuf }
| eof               { T_eof }
| '('               { T_lparen }
| ')'               { T_rparen }
| '{'               { T_lbrace }
| '}'               { T_rbrace }
| '<'               { T_langle }
| '>'               { T_rangle }
| ','               { T_comma }
| '.'               { T_period }
| ';'               { T_semicolon }
| '='               { T_assign }
| (letter | '_') (letter | digit | '_')*
  { let ident = L.lexeme lexbuf in
    try
      Hashtbl.find keyword_table ident
    with
      Not_found -> T_ident ident }
| ("'")(letter | '_') (letter | digit | '_')*
  {let ident = L.lexeme lexbuf in
      T_tyvar ident}
|  _
  { error (P.sprintf "unknown token %s near characters" (L.lexeme lexbuf)) lexbuf }

and comment = parse
| eof               { warning "unterminated comment" lexbuf }
| "/*"              { comment lexbuf }
| "*/"              { () }
| _                 { comment lexbuf }
