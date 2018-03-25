open Pprinting

<<<<<<< HEAD
let () = 
  let lexbuf = Lexing.from_channel stdin in
  let ast = Parser.program Scanner.tokenize lexbuf in
  let _ = Semant.check ast in
  print_string (Pprinting.string_of_program ast)

(* 
 type action = Ast | Sast | LLVM_IR | Compile
=======
type action = Ast | Sast | LLVM_IR | Compile
>>>>>>> 3038e3f34d6a8de2781c466b59de3a7f5abe05a7

let () = 
  let action = ref Compile in
  let set_action a () = action := a in
  let speclist = [
    ("-a", Arg.Unit (set_action Ast), "Print the AST");
    ("-s", Arg.Unit (set_action Sast), "Print the SAST");
    ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
    ("-c", Arg.Unit (set_action Compile),
      "Check and print the generated LLVM IR (default)");
  ] in
  let usage_msg = "usage: ./heckell [-a|-s|-l|-c] [file.hck]" in
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;

  let lexbuf = Lexing.from_channel !channel in
  let ast = Parser.program Scanner.tokenize lexbuf in
  match !action with
  | Ast -> print_string (Pprinting.string_of_program ast)
  | _ -> let sast = Semant.check ast in
    match !action with
    | Ast -> ()
    | Sast -> print_string (Pprinting.string_of_sprogram sast)
    | _ -> ()
  (* let lexbuf = Lexing.from_channel stdin in
  let ast = Parser.program Scanner.tokenize lexbuf in
  let sast = Semant.check ast in
  let m = Codegen.translate sast in
  Llvm_analysis.assert_valid_module m;
  print_string (Llvm.string_of_llmodule m)
  (* print_string (Pprinting.string_of_program ast) *)
*)
