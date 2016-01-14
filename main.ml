open Ast
module CT = Map.Make (struct
                        type t = Tycon.t
                        let compare = compare
                      end)
module TypeCheck = TypeCheck.Make (struct
                                    module CT = CT
                                   end)
module RegionTypeInfer = RegionTypeInfer.Make (struct
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
    List.fold_right
      (fun k ct ->
         begin
           let x = Class.tycon k in 
             if CT.mem x ct then 
               failwith @@ "The class"^(Tycon.toString x)
                  ^"has already been defined.\n" 
             else ();
           CT.add x k ct
         end) ks ct

let main _ =
  let files = ref [] in
  Arg.parse
    [("-debug",
      Arg.Unit (fun () -> Control.debug := true),
      "true iff debug")]
    (fun file -> files := !files @ [file])
    ("broomc: Broom compiler. Copyright(c) Microsoft Research\n" ^
     "  usage: broomc [<option>] <file0> <file1> ...\n");
  List.iter begin fun f ->
    let channel = open_in f in
    try
      let lexbuff = Lexing.from_channel channel in
      let classes = Parser.prog Lexer.token lexbuff in
      (*
       * Tycons in the same order they appear in the source text.
       *)
      let tycons = List.map Class.tycon classes in
      let ct = makeClassTable classes in
      let _ = if false then
        begin
          print_string "Parser output:\n";
          CT.iter (fun tycon k -> 
            begin
              Class.print k;
            end ) ct;
        end else () in
      let ct' = TypeCheck.doIt ct in
      let _ = if false then
        begin
          print_string "Post FGJ Type checking:\n";
          CT.iter (fun tycon k -> 
            begin
              Class.print k;
            end ) ct';
        end else () in
      let ct'' = RegionTypeInfer.doIt tycons ct' in
        begin
          print_string "\nPost Region Type Inference:\n";
          List.iter (fun tycon -> 
            begin
              RegionAst.Class.print (CT.find tycon ct'');
            end ) tycons;
        end
    with
      e -> close_in channel; raise e
  end !files;;

let t1 = Sys.time() in
let _ = main () in
let t2 = Sys.time() in
  Printf.printf "Execution time: %fs\n" (t2 -. t1)
