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
  let i1_t       = L.i1_type        context in
  let f32_t      = L.float_type     context in
  let str_t      = L.pointer_type   i8_t    in
  let intp_t     = L.pointer_type   i32_t   in
  let void       = L.void_type      context in
  (* Create an LLVM module -- this is a "container" into which we'll 
     generate actual code *)
  let the_module = L.create_module context "Heckell" in

  (* Convert Heckell types to LLVM types *)
  let ltype_of_typ = function
      A.PrimTyp(A.Int)  -> i32_t
    | A.PrimTyp(A.Char) -> i8_t
    | A.PrimTyp(A.Bool) -> i1_t
    | A.PrimTyp(A.Real) -> f32_t
    | A.String          -> str_t
    | A.Set(_)          -> str_t
    | t -> raise (Failure ("Type " ^ string_of_typ t ^ " not implemented yet"))
  in

  (* Declare a "printf" function to implement MicroC's "print". *)
  let printf_t : L.lltype = 
      L.var_arg_function_type i32_t [| str_t |] in
  let printf_func : L.llvalue = 
     L.declare_function "printf" printf_t the_module in 

  (* str_t represents any pointer as char, void, hset_head pointers 
     are all i8 pointers to LLVM *)
  (* init_hset returns hset_head pointer (NULL) *)
  let init_hset_t : L.lltype = 
      L.var_arg_function_type str_t [| (* void *) |] in
  let init_hset_func : L.llvalue =
      L.declare_function "init_hset" init_hset_t the_module in
   
  (* add_val returns new hset_head pointer and
     takes string of value, void pointer to value,
     type string, and original hset_head pointer*)
  let add_val_t : L.lltype = 
      L.var_arg_function_type str_t [| str_t; str_t; str_t |] in
  let add_val_func : L.llvalue =
      L.declare_function "add_val" add_val_t the_module in

  (* del_val returns new hset_head pointer and
     takes string of value, type string, and original hset_head pointer *)
  let del_val_t : L.lltype = 
      L.var_arg_function_type str_t [| str_t; str_t; str_t |] in
  let del_val_func : L.llvalue =
      L.declare_function "del_val" del_val_t the_module in

  (* destroy_hset takes hset_head pointer to be destroyed *)
  let destroy_hset_t : L.lltype = 
      L.var_arg_function_type void [| str_t |] in
  let destroy_hset_func : L.llvalue =
      L.declare_function "destroy_hset" destroy_hset_t the_module in

  let print_hset_t : L.lltype =
      L.var_arg_function_type void [| str_t |] in
  let print_hset_func : L.llvalue =
      L.declare_function "print_hset" print_hset_t the_module in

  let to_imp str = raise (Failure ("Not yet implemented: " ^ str)) in

  (* Make the LLVM module "aware" of the main function *)
  let main_ty = L.function_type (ltype_of_typ (A.PrimTyp A.Int)) [||] in
  let the_function = L.define_function "main" main_ty the_module in
  (* Create an Instruction Builder, which points into a basic block
    and determines where the next instruction should be placed *)
  let builder = L.builder_at_end context (L.entry_block the_function) in

  (* Create a pointer to a format string for printf *)
  let int_format_str      = L.build_global_stringptr "%d\n" "fmt" builder 
  and str_format_str      = L.build_global_stringptr "%s\n" "fmt_str" builder
  and set_int_format_str  = L.build_global_stringptr "%d "  "set_str" builder
  and setl_format_str     = L.build_global_stringptr "{ "   "setl_str" builder
  and setr_format_str     = L.build_global_stringptr "}\n"  "setr_str" builder
  and int_str             = L.build_global_stringptr "Int"  "int" builder
  and real_str            = L.build_global_stringptr "Real" "real" builder
  and bool_str            = L.build_global_stringptr "Bool" "bool" builder
  and char_str            = L.build_global_stringptr "Char" "char" builder
  in
  let strtype_of_typ = function
      A.PrimTyp(A.Int)  -> int_str
    | A.PrimTyp(A.Char) -> char_str
    | A.PrimTyp(A.Bool) -> bool_str
    | A.PrimTyp(A.Real) -> real_str
  in
  let lookup n map = try StringMap.find n map
                     with Not_found -> to_imp "ERROR: asn not found."
  in
  let build_statements var_map stmt = 
    let rec add_list_vals (slist: sexpr list) t hset_ptr = match slist with
      | [] -> raise (Failure "empty list added to set") 
      | [ se ] -> let val_addr = L.build_alloca (ltype_of_typ t) "temp" builder in
          let _ = L.build_store (expr builder se) val_addr builder in
          let bitcast = L.build_bitcast val_addr str_t "bitcast" builder in 
          L.build_call add_val_func [| bitcast; (strtype_of_typ t); hset_ptr |] "add_val" builder 
      | head :: tail -> 
          let new_hset_ptr = 
            let val_addr = L.build_alloca (ltype_of_typ t) "temp" builder in
            let _ = L.build_store (expr builder head) val_addr builder in
            let bitcast = L.build_bitcast val_addr str_t "bitcast" builder in 
            L.build_call add_val_func [| bitcast; (strtype_of_typ t); hset_ptr |] "add_val" builder 
          in add_list_vals tail t new_hset_ptr 
    and expr builder (_, e) = match e with
        SLit i -> L.const_int i32_t i (* Generate a constant integer *)
      | SCharLit c -> L.const_int i8_t (Char.code c)
      | SBoolLit b -> L.const_int i1_t 1
      | SRealLit r -> L.const_float f32_t r
      | SSetLit sl -> let hset_ptr = L.build_call init_hset_func [| |] "init_hset" builder 
          in add_list_vals sl (fst (List.hd sl)) hset_ptr  
      | SId s -> L.build_load (lookup s var_map) s builder
      | SStringLit s -> 
        L.build_global_stringptr s ".str" builder
      | SFuncCall ("print", e) -> L.build_call printf_func [| int_format_str ; (expr builder e) |] "printf" builder
      | SFuncCall ("print_string", e) -> L.build_call printf_func [| str_format_str ; (expr builder e) |] "printf" builder
      | SFuncCall ("print_set", e) -> match snd e with
          | SSetLit(sl) -> L.build_call print_hset_func [| (expr builder e) |] "" builder
          | SId(n) -> let addr = lookup n var_map in 
              let hset_ptr = L.build_load addr "var_ptr" builder 
              in L.build_call print_hset_func [| hset_ptr |] "" builder
          | _ -> raise (Failure "welp")
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
    match stmt with
        | SExpr e -> ignore(expr builder e); var_map
        (* Handle a declaration *)
        | SDecl (n, A.Set(t)) ->
              (* addr should be return value of init_hset *)
              let hset_ptr = L.build_call init_hset_func [| |] "init_hset" builder
              and addr = L.build_alloca str_t n builder
              (* throws error if build_store in add vs ignore(build_store) ; add? *)
              in ignore(L.build_store hset_ptr addr builder); StringMap.add n addr var_map 
        | SDecl (n, t) -> let addr = L.build_alloca (ltype_of_typ t) n builder
              in StringMap.add n addr var_map (* TODO DONT IGNORE THIS *)
        | SAsn (n, (A.Set(t), SSetLit(sl))) -> 
              let addr = StringMap.find n var_map 
              in let curr_hset_ptr = L.build_load addr "hset_ptr" builder 
              in let final_hset_ptr = add_list_vals sl t curr_hset_ptr 
              in ignore(L.build_store final_hset_ptr addr builder); var_map
        (* add val one by one and replace addr in var_map with new set *)
        | SAsn (n, sexpr) -> let addr = StringMap.find n var_map 
                  in let e' = expr builder sexpr 
                  in ignore(L.build_store e' addr builder); var_map (* TODO: should this really be ignored? *)

  in List.fold_left build_statements StringMap.empty stmt_list;
  (*in List.iter (build_statements StringMap.empty) stmt_list;*)
  ignore(L.build_ret (L.const_int i32_t 0) builder);
  the_module
