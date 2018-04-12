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
let translate (stmt_list) =
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
    | A.String         -> str_t
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
  and str_format_str = L.build_global_stringptr "%s\n" "fmt_str" builder in

  let lookup n map = try StringMap.find n map
                     with Not_found -> raise(Failure ("SAADDD not in map"))
  in
  let build_statements var_map stmt = 
    let rec expr builder (_, e) = match e with
        SLit i -> L.const_int i32_t i (* Generate a constant integer *)
      | SId s -> L.build_load (lookup s var_map) s builder
      | SStringLit s -> 
        L.build_global_stringptr s ".str" builder
      | SFuncCall ("print", e) -> L.build_call printf_func [| int_format_str ; (expr builder e) |] "printf" builder
      | SFuncCall ("print_string", e) -> L.build_call printf_func [| str_format_str ; (expr builder e) |] "printf" builder
      | SBinop (e1, op, e2) ->
        let (t, _) = e1
        and e1' = expr builder e1
        and e2' = expr builder e2 in
        if t = A.PrimTyp(A.Real) then (match op with 
          A.Add     -> L.build_fadd
        | A.Sub     -> L.build_fsub
        | A.Mul     -> L.build_fmul
        | A.Div     -> L.build_fdiv 
        | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
        | A.Neq     -> L.build_fcmp L.Fcmp.One
        | A.Less    -> L.build_fcmp L.Fcmp.Olt
        | A.Leq     -> L.build_fcmp L.Fcmp.Ole
        | A.Greater -> L.build_fcmp L.Fcmp.Ogt
        | A.Geq     -> L.build_fcmp L.Fcmp.Oge
        | A.And | A.Or -> raise (Failure "internal error: semant should have rejected and/or on float")
        ) e1' e2' "tmp" builder 
        else (match op with
        | A.Add     -> L.build_add
        | A.Sub     -> L.build_sub
        | A.Mul     -> L.build_mul
        | A.Div     -> L.build_sdiv
        | A.And     -> L.build_and
        | A.Or      -> L.build_or
        | A.Equal   -> L.build_icmp L.Icmp.Eq
        | A.Neq     -> L.build_icmp L.Icmp.Ne
        | A.Less    -> L.build_icmp L.Icmp.Slt
        | A.Leq     -> L.build_icmp L.Icmp.Sle
        | A.Greater -> L.build_icmp L.Icmp.Sgt
        | A.Geq     -> L.build_icmp L.Icmp.Sge
        ) e1' e2' "tmp" builder
      | SUniop(op, e) ->
        let (t, _) = e and e' = expr builder e in
        (match op with
          A.Neg when t = A.PrimTyp(A.Real) -> L.build_fneg 
        | A.Neg                            -> L.build_neg
        ) e' "tmp" builder
      | _ -> to_imp "" (* TODO: implemnet variable reference *)
    in 
    let rec stmt_builder builder s = match s with
        | SExpr e -> ignore(expr builder e)
        (* Handle a declaration *)
        | SDecl (n, t) -> let addr = L.build_alloca (ltype_of_typ t) n builder
                  in ignore(StringMap.add n addr var_map)
        | SAsn (n, sexpr) -> let addr = StringMap.find n var_map
                  in ignore(L.build_store (expr builder sexpr) addr builder) (* TODO: should this really be ignored? *)
    in stmt_builder builder stmt
  in List.iter (build_statements StringMap.empty) stmt_list;
  ignore(L.build_ret (L.const_int i32_t 0) builder);
  the_module
