(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

(* We'll refer to Llvm and Ast constructs with module names *)
module L = Llvm
module A = Ast
open Pprinting
open Sast 

module StringMap = Map.Make(String)

(* Code Generation from the SAST. Returns an LLVM module if successful,
   throws an exception if something is wrong. *)
let translate (statement_list) =
  let context    = L.global_context () in
  (* Add types to the context so we can use them in our LLVM code *)
  let i32_t      = L.i32_type       context in
  let i8_t       = L.i8_type        context in
  let str_t      = L.pointer_type   i8_t    in
  (* Create an LLVM module -- this is a "container" into which we'll 
     generate actual code *)
  let the_module = L.create_module context "Heckell" in

  (* Convert Heckell types to LLVM types *)
  let ltype_of_typ = function
      A.PrimTyp(A.Int) -> i32_t
    | A.String       -> str_t
    | t -> raise (Failure ("Type " ^ string_of_typ t ^ " not implemented yet"))
  in

  (* Declare a "printf" function to implement MicroC's "print". *)
  let printf_t : L.lltype = 
      L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue = 
     L.declare_function "printf" printf_t the_module in 

  let to_imp str = raise (Failure ("Not yet implemented: " ^ str)) in

  (* Make the LLVM module "aware" of the main function *)
  let main_ty = L.function_type (ltype_of_typ (A.PrimTyp A.Int)) [||] in
  let the_function = L.define_function "main" main_ty the_module in
  (* Create an Instruction Builder, which points into a basic block
    and determines where the next instruction should be placed *)
  let builder = L.builder_at_end context (L.entry_block the_function) in
  (* Create a pointer to a format string for printf *)
  let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder 
  and str_format_str = L.build_global_stringptr "%s\n" "fmt_str" builder
  in

  (* Generate the instructions for a trivial "main" function *)
  let build_function fdecl =
    (* Generate LLVM code for a call to Heckell's "print" *)
    let rec exprb builder (_, e) = match e with
        SLit i -> L.const_int i32_t i (* Generate a constant integer *)
      | SStringLit s -> 
        L.build_global_stringptr s ".str" builder
      | SFuncCall ("print", (se_t, se)) -> ( match se_t with (* Generate a call instruction *)
        | A.PrimTyp(A.Int) -> L.build_call printf_func [| int_format_str ; (exprb builder (se_t, se)) |]
          "printf" builder
        | A.String -> L.build_call printf_func [| str_format_str ; (exprb builder (se_t, se)) |]
          "printf" builder
        | _ -> to_imp "")
      (* Throw an error for any other expressions *)
      | _ -> to_imp ""
    in match fdecl with
    | SExpr e -> ignore(exprb builder e)
    | _ -> ()

  in List.iter build_function statement_list; L.build_ret (L.const_int i32_t 0) builder;
  the_module
