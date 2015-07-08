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
  "class Pair extends Object\n"
^ "{\n"
^ "  Object fst;\n"
^ "  Object snd;\n"
^ "  Pair(Object fst, Object snd) {\n"
^ "    this.fst = fst;\n"
^ "    this.snd = snd;\n"
^ "  }\n"
^ "  Pair swap() {\n"
^"     Object temp = this.fst;\n"
^"     this.fst = this.snd;\n"
^"     this.snd = temp;\n"
^"   }\n"
^ "}\n";;

let pairTycon = Tycon.make $ Id.fromString "Pair"
let pairType = Type.mkApp (pairTycon,[])
let this = Var.fromString "this"
let thisExpr = Expr.make (Expr.Var this, Type.Unknown)
let fstVar = Var.fromString "fst"
let sndVar = Var.fromString "snd"
let fstExpr = Expr.make (Expr.Var fstVar, Type.Unknown)
let sndExpr = Expr.make (Expr.Var sndVar, Type.Unknown)

let expected =
  Class.make 
    ~tycon: pairTycon
    ~tyvars: []
    ~super: Type.Object
    ~ctors: [Con.make 
                ~tycon: pairTycon
                ~params: [(fstVar, Type.Object); 
                          (sndVar, Type.Object)]
                ~body: (Stmt.Seq [Stmt.FieldSet(thisExpr, 
                                               Field.fromString "fst", 
                                               fstExpr);
                                  Stmt.FieldSet(thisExpr,
                                               Field.fromString "snd", 
                                               sndExpr)])]
    ~fields: [(Field.fromString "fst", Type.Object);
              (Field.fromString "snd", Type.Object)]
    ~methods: [(Method.make
                  ~name: (MethodName.fromString "swap")
                  ~params: []
                  ~body: (Stmt.Seq [Stmt.VarDec (Type.Object,
                                                Var.fromString "temp",
                                                Expr.make (Expr.FieldGet (thisExpr,
                                                                          Field.fromString "fst"),
                                                           Type.Unknown));
                                   Stmt.FieldSet (thisExpr,
                                                  Field.fromString "fst",
                                                  Expr.make (Expr.FieldGet (thisExpr,
                                                                            Field.fromString "snd"),
                                                           Type.Unknown));
                                   Stmt.FieldSet (thisExpr,
                                                  Field.fromString "snd",
                                                  Expr.make (Expr.Var (Var.fromString "temp"),
                                                             Type.Unknown))])
                  ~ret_type: pairType)]

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
    Printf.printf "Parser test\n";
    run_test_tt test_suite
  end
