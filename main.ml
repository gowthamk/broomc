open Ast
module CT = Map.Make (struct
                        type t = Tycon.t
                        let compare = compare
                      end)
module TypeCheck = TypeCheck.Make (struct
                                    module CT = CT
                                   end)
let non_existence_type = Type.Unknown
let base_klass_tycon = Tycon.make @@ Id.fromString "Object"
let base_klass_type = Type.mkApp (base_klass_tycon,[])
let base_klass = Class.make
  ~tycon: base_klass_tycon
  ~tyvars: []
  ~super: non_existence_type
  ~fields: []
  ~ctors: []
  ~methods: []

let makeClassTable ks =
  let ct = CT.singleton base_klass_tycon base_klass in
    List.fold_left 
      (fun ct k ->
         begin
           let x = Class.tycon k in 
             if CT.mem x ct then 
               failwith @@ "The class"^(Tycon.toString x)
                  ^"has already been defined.\n" 
             else ();
           CT.add x k ct
         end) ct ks

let _ =
  let files = ref [] in
  Arg.parse
    []
    (fun file -> files := !files @ [file])
    ("broomc: Broom compiler. Copyright(c) Microsoft Research\n" ^
     "  usage: broomc [<option>] <file0> <file1> ...\n");
  List.iter begin fun f ->
    let channel = open_in f in
    try
      let lexbuff = Lexing.from_channel channel in
      let classes = Parser.prog Lexer.token lexbuff in
      let ct = makeClassTable classes in
      let _ =
        begin
          print_string "Parser output:\n";
          CT.iter (fun tycon k -> 
            begin
              Class.print k;
              Format.printf "@\n";
            end ) ct;
        end in
      let ct' = TypeCheck.doIt ct in
        begin
          print_string "Post FGJ Type checking:\n";
          CT.iter (fun tycon k -> 
            begin
              Class.print k;
              Format.printf "@\n";
            end ) ct';
        end
    with
      e -> close_in channel; raise e
  end !files
