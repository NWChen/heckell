(* module L = Llvm *)
module A = Ast
open Sast 


(* let translate (statements) =
  let context    = L.global_context () in
  (* Add types to the context so we can use them in our LLVM code *)
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  (* Create an LLVM module -- this is a "container" into which we'll 
     generate actual code *)
  and the_module = L.create_module context "heckell" in

  let ltype_of_typ = function
    A.Int   -> i32_t
  in

  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  let build_statements stmt =
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in

    let rec expr builder (_, e) = match e with
      SLiteral i -> L.const_int i32_t i
      | SCall ("print", [e]) | SCall ("printb", [e]) ->
          L.build_call printf_func [| int_format_str ; (expr builder e) |] "printf" builder

  in
  List.iter build_statements statements in
  the_module *)