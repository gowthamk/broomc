%{
 (* header *)
 open Ast

 exception ParseError
 type 'a revlist = Snoc of 'a revlist * 'a | Nil

 let rec to_list = function
 | Snoc(xs, x) -> x :: (to_list xs)
 | Nil         -> []

 let to_list_with_rev xs =
   List.rev (to_list xs)
 
 let rec of_list = function
 | x :: xs -> Snoc(of_list xs, x)
 | []      -> Nil

 let rec of_list_with_rev xs =
   of_list (List.rev xs)

  module List = 
    struct
      include List
      let optmap f l = map f l |> filter (function None -> false
                                                 | _ -> true)
                               |> map (function (Some x) -> x
                                              | None -> failwith "Impossible Case")
    end

  let ($) f x = x |> f

%}
%token T_eof
%token T_lparen
%token T_rparen
%token T_lbrace
%token T_rbrace
%token T_langle
%token T_rangle
%token T_semicolon
%token T_period
%token T_comma
%token T_assign
%token <string> T_ident
%token <string> T_tyvar
%token <string> T_un_op
%token <string> T_bin_op
%token <int> T_int
%token T_keyword_class
%token T_keyword_extends
%token T_keyword_new
%token T_keyword_super
%token T_keyword_if
%token T_keyword_else
%token T_keyword_while
%token T_keyword_return
%token T_keyword_let
%token T_keyword_letregion
%token T_keyword_open
%token T_keyword_openalloc
%token T_keyword_int
%token T_keyword_bool
%token T_keyword_void
%token T_keyword_null
%token T_keyword_true
%token T_keyword_false
%right T_comma

/* Starting non-terminal is prog. Its type is Ast.Class.t list */
%type <Ast.Class.t list> prog 
%start                   prog
/* (Optional) types for other non-terminals */
%type <(Ast.Tyvar.t * Ast.Type.t) list> tyvardecs
%type <(Ast.Tyvar.t * Ast.Type.t) list> tyvardecseq
%type <(Ast.Tyvar.t * Ast.Type.t)> tyvardec
%type <Ast.Type.t list> typseq
%type <Ast.Tycon.t> tycon
%type <Ast.Type.t> classtyp
%type <Ast.Type.t> typ
%type <Ast.Field.t> field
%type <Ast.Var.t> var
%type <Ast.MethodName.t> methodname
%type <Ast.Expr.t> expr
%type <Ast.Stmt.t> stmt
%type <(Ast.Var.t * Ast.Type.t) list> params

%%
/* BNF Rules */

prog:
  | class_definition_list T_eof
    { to_list_with_rev $1 }

class_definition_list:
  | class_definition_list class_definition
    { Snoc($1,$2) }
  |
    { Nil }

tycon:
 | T_ident 
   {Ast.Tycon.make $ Id.fromString $1}

classtyp : 
 | tycon 
    {if Tycon.isObject $1
    then Ast.Type.Object 
    else Ast.Type.mkApp ($1,[])}
 | tycon T_langle typseq T_rangle 
    {Ast.Type.mkApp ($1,$3)}

typ:
 | T_keyword_int
    {Ast.Type.Int}
 | T_keyword_bool
    {Ast.Type.Bool}
 | T_keyword_void
    {Ast.Type.Unit}
 | classtyp 
    {$1}
 | tyvar
    {Ast.Type.var $1}

var:
 | T_ident {Ast.Var.fromString $1}

field:
 | T_ident {Ast.Field.fromString $1}

methodname:
 | T_ident {Ast.MethodName.fromString $1}

typseq: 
 | typ T_comma typseq
    {$1 :: $3}
 | typ
   {[$1]}

tyvar:
 | T_tyvar {Tyvar.fromString $1} 

tyvardec: 
 | tyvar T_keyword_extends classtyp
    {($1, $3)}

tyvardecseq :
 | tyvardec T_comma tyvardecseq
    {$1 :: $3}
 | tyvardec
    {[$1]}

tyvardecs:
 | T_langle tyvardecseq T_rangle
    {$2}
 |
    {[]}

class_definition:
  | T_keyword_class tycon tyvardecs T_keyword_extends classtyp
    T_lbrace
      memberdecs
    T_rbrace
    { let tycon = $2 in
      let tyvardecs = $3 in
      let super = $5 in
      let fields = List.optmap (function (`Field (f,t)) -> Some (f,t)
                                       | _ -> None) $7 in
      let ctors = List.optmap (function (`Ctor c) -> Some c
                                       | _ -> None) $7 in
      let meths = List.optmap (function (`Method m) -> Some m
                                       | _ -> None) $7 in
        Class.make tycon tyvardecs super fields ctors meths
    }

memberdecs:
  | memberdec memberdecs
    { $1 :: $2 }
  |   {[]}

memberdec:
  | fielddec { `Field $1 }
  | ctordec { `Ctor $1 }
  | methdec { `Method $1 }

fielddec:
  | typ field T_semicolon {($2,$1)}

param:
  | typ var {($2,$1)}

params:
 | param T_comma params {$1 :: $3}
 | param {[$1]}

ctordec:
  | tycon T_lparen T_rparen
    T_lbrace
      stmt
    T_rbrace
    { Ast.Con.make $1 [] $5}
  | tycon T_lparen params T_rparen
    T_lbrace
      stmt
    T_rbrace
    { Ast.Con.make $1 $3 $6}

methdec:
  | typ methodname T_lparen T_rparen
    T_lbrace
      stmt
    T_rbrace
    { Ast.Method.make $2 [] $6 $1 }
  | typ methodname T_lparen params T_rparen
    T_lbrace
      stmt
    T_rbrace
    { Ast.Method.make $2 $4 $7 $1 }

unary_op:
  | T_un_op { if $1 = "!" then Prim.Not
              else failwith "Unknown unary op"}
binary_op:
  | T_bin_op 
      { match $1 with
          | "+" -> Prim.Plus | "-" -> Prim.Minus
          | "*" -> Prim.Mult | "/" -> Prim.Div
          | "==" -> Prim.Equal | ">" -> Prim.GT
          | "<" -> Prim.LT | "&&" -> Prim.And
          | "||" -> Prim.Or 
          | _ -> failwith "Unknown binop"}

exprseq:
  | expr T_comma exprseq {$1 :: $3}
  | expr {[$1]}

args:
  | exprseq {$1}
  |    {[]}

expr:
  | exprnode {Expr.make ($1,Type.Unknown)}
  /*(*| T_lparen exprnode T_rparen{Expr.make ($2,Type.Unknown)}*)*/

exprnode:
  | T_keyword_null {Ast.Expr.Null}
  | T_int {Ast.Expr.Int $1}
  | T_keyword_true {Ast.Expr.Bool true}
  | T_keyword_false {Ast.Expr.Bool false}
  | var { Ast.Expr.Var $1 }
  | expr T_period field
    { Ast.Expr.FieldGet($1, $3) }
  | expr T_period methodname T_lparen args T_rparen
    { Ast.Expr.MethodCall($1, $3, $5) }
  | T_keyword_new classtyp T_lparen args T_rparen
    { match $2 with 
      | Ast.Type.ConApp (tycon,targs) -> Expr.New(tycon,targs,$4) 
      | _ -> raise ParseError}
  | unary_op T_lparen expr T_rparen {Expr.UnOpApp ($1,$3)}
  | expr binary_op expr {Expr.BinOpApp ($1,$2,$3)}
  | T_lparen exprnode T_rparen {$2}

stmt: 
  | atomstmtseq {Ast.Stmt.Seq $1}

atomstmtseq:
  | atomstmt atomstmtseq {$1 :: $2}
  |   {[]}

atomstmt:
  | typ var T_assign expr T_semicolon 
      {Ast.Stmt.VarDec ($1,$2,$4)}
  | T_keyword_let typ var T_assign expr T_semicolon 
      {Ast.Stmt.VarDec ($2,$3,$5)}
  | var T_assign expr T_semicolon 
      {Ast.Stmt.Assn ($1,$3)}
  | expr T_period field T_assign expr T_semicolon 
      {let lhsExp = Expr.make (Expr.FieldGet ($1,$3), Type.Unknown) in
        Ast.Stmt.FieldSet (lhsExp,$5)}
  | expr T_semicolon 
      {Ast.Stmt.Expr $1}
  | T_keyword_if T_lparen expr T_rparen T_lbrace stmt T_rbrace
      {let grd = $3 in
       let tstmt = $6 in
       let fstmt = Ast.Stmt.Seq [] in 
         Ast.Stmt.ITE (grd,tstmt,fstmt)}
  | T_keyword_if T_lparen expr T_rparen T_lbrace stmt T_rbrace
                         T_keyword_else T_lbrace stmt T_rbrace
      {let grd = $3 in
       let tstmt = $6 in
       let fstmt = $10 in 
         Ast.Stmt.ITE (grd,tstmt,fstmt)}
  | T_keyword_while T_lparen expr T_rparen T_lbrace stmt T_rbrace 
      {Ast.Stmt.While ($3,$6)}
  | T_keyword_return expr T_semicolon
      {Ast.Stmt.Expr $2}
  | T_keyword_letregion T_lbrace stmt T_rbrace
      {Ast.Stmt.LetRegion $3}
  | T_keyword_open T_lparen var T_rparen T_lbrace stmt T_rbrace 
      {Ast.Stmt.Open (Expr.make (Expr.Var $3,Type.Unknown),$6)}
  | T_keyword_openalloc T_lparen var T_rparen T_lbrace stmt T_rbrace 
      {Ast.Stmt.OpenAlloc (Expr.make (Expr.Var $3,Type.Unknown),$6)}
