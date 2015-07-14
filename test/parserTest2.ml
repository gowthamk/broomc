(*
 * Copyright (c) 2014 - 2015 Takahisa Watanabe <linerlock@outlook.com> All rights reserved.
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 *)
open OUnit
open Lexer
open Parser
open Ast

let ($) f x = x |> f

let target =
  "class Pair<'a extends Object, 'b extends Object> extends Object\n"
^ "{\n"
^ "  'a fst;\n"
^ "  'b snd;\n"
^ "  Pair('a fst, 'b snd) {\n"
^ "    this.fst = fst;\n"
^ "    this.snd = snd;\n"
^ "  }\n"
^ "  Pair<'b,'a> swap() {\n"
^"     new Pair<'b,'a>(this.snd,this.fst);\n"
^"   }\n"
^ "}\n";;

let pairTycon = Tycon.make $ Id.fromString "Pair"
let (tyvA,tyvB)  = (Tyvar.fromString "'a", Tyvar.fromString "'b")
let tyvardecs = List.map (fun tyv -> (tyv,Type.Object)) [tyvA; tyvB]
let (tyA, tyB) = (Type.var tyvA, Type.var tyvB)
let pairABType = Type.mkApp (pairTycon,[tyA;tyB])
let pairBAType = Type.mkApp (pairTycon,[tyB;tyA])
let this = Var.fromString "this"
let thisExpr = Expr.make (Expr.Var this, Type.Unknown)
let fstVar = Var.fromString "fst"
let sndVar = Var.fromString "snd"
let fstExpr = Expr.make (Expr.Var fstVar, Type.Unknown)
let sndExpr = Expr.make (Expr.Var sndVar, Type.Unknown)
let thisFstExpr = Expr.make (Expr.FieldGet (thisExpr,
                                            Field.fromString "fst"),
                             Type.Unknown)
let thisSndExpr = Expr.make (Expr.FieldGet (thisExpr,
                                            Field.fromString "snd"),
                             Type.Unknown)

let expected =
  Class.make 
    ~tycon: pairTycon
    ~tyvars: tyvardecs
    ~super: Type.Object
    ~ctors: [Con.make 
                ~tycon: pairTycon
                ~params: [(fstVar, tyA); 
                          (sndVar, tyB)]
                ~body: (Stmt.Seq [Stmt.FieldSet(thisFstExpr, 
                                               fstExpr);
                                  Stmt.FieldSet(thisSndExpr, 
                                               sndExpr)])]
    ~fields: [(Field.fromString "fst", tyA);
              (Field.fromString "snd", tyB)]
    ~methods: [(Method.make
                  ~name: (MethodName.fromString "swap")
                  ~params: []
                  ~body: (Stmt.Seq [(Stmt.expr $ 
                                       Expr.make (Expr.New (pairTycon,
                                                            [tyB; tyA],
                                                            [thisSndExpr; thisFstExpr]),
                                                     Type.Unknown))])
                  ~ret_type: pairBAType)]

let parse s =
  Parser.prog Lexer.token (Lexing.from_string s)

let parse_test =
    "parse_test" >:: (fun () ->
        assert_equal [expected] (parse target))

let test_suite = 
  "Parser" >::: [
    parse_test
  ]

let _ = 
  begin
    Printf.printf "Parser test2\n";
    run_test_tt test_suite
  end
