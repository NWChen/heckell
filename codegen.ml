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
  let ptr_t      = L.pointer_type   i8_t    in
  let intp_t     = L.pointer_type   i32_t   in
  let void_t     = L.void_type      context in
  let i1_t       = L.i1_type        context in
  (* Create an LLVM module -- this is a "container" into which we'll 
     generate actual code *)
  let the_module = L.create_module context "Heckell" in

  (* Convert Heckell types to LLVM types *)
  let rec ltype_of_typ = function
      A.PrimTyp(A.Int)  -> i32_t
    | A.PrimTyp(A.Char) -> i8_t
    | A.PrimTyp(A.Bool) -> i8_t  (* for compatability with c types, can't use i1 *)
    | A.PrimTyp(A.Real) -> f32_t
    | A.String          -> str_t
    | A.Set(_)          -> str_t
    | A.Tuple(typs)     -> 
      L.pointer_type (L.struct_type context (Array.of_list (List.map ltype_of_typ typs)))
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

  let hset_union_t : L.lltype = 
      L.var_arg_function_type str_t [| str_t; str_t; str_t |] in
  let hset_union_func : L.llvalue =
      L.declare_function "hset_union" hset_union_t the_module in

  let hset_diff_t : L.lltype = 
      L.var_arg_function_type str_t [| str_t; str_t; str_t |] in
  let hset_diff_func : L.llvalue =
      L.declare_function "hset_diff" hset_diff_t the_module in

  (* destroy_hset takes hset_head pointer to be destroyed *)
  let destroy_hset_t : L.lltype = 
      L.var_arg_function_type void_t [| str_t |] in
  let destroy_hset_func : L.llvalue =
      L.declare_function "destroy_hset" destroy_hset_t the_module in

  let print_hset_t : L.lltype =
      L.var_arg_function_type void_t [| str_t |] in
  let print_hset_func : L.llvalue =
      L.declare_function "print_hset" print_hset_t the_module in

  let string_of_t : L.lltype =
      L.function_type str_t [| ptr_t ; str_t |] in
  let string_of_func : L.llvalue = 
      L.declare_function "string_of" string_of_t the_module in 
  
  let string_interpolation_t : L.lltype =
      L.var_arg_function_type str_t [| str_t ; i32_t |] in
  let string_interpolation_func : L.llvalue =
      L.declare_function "string_interpolation" string_interpolation_t the_module in

  let free_args_t : L.lltype =
      L.var_arg_function_type void_t [| i32_t |] in
  let free_args_func : L.llvalue =
      L.declare_function "free_args" free_args_t the_module in

  let to_imp str = raise (Failure ("Not yet implemented: " ^ str)) in

  (* Make the LLVM module "aware" of the main function *)
  let main_ty = L.function_type (ltype_of_typ (A.PrimTyp A.Int)) [||] in
  let the_function = L.define_function "main" main_ty the_module in
  (* Create an Instruction Builder, which points into a basic block
    and determines where the next instruction should be placed *)
  let builder = L.builder_at_end context (L.entry_block the_function) in

  (* Create a pointer to a format string for printf *)
  let str_format_str      = L.build_global_stringptr "%s\n" "fmt_str" builder
  and int_str             = L.build_global_stringptr "Int"  "int" builder
  and real_str            = L.build_global_stringptr "Real" "real" builder
  and bool_str            = L.build_global_stringptr "Bool" "bool" builder
  and char_str            = L.build_global_stringptr "Char" "char" builder
  and string_str          = L.build_global_stringptr "String" "string" builder
  in
  let strtype_of_typ = function
      A.PrimTyp(A.Int)  -> int_str
    | A.PrimTyp(A.Char) -> char_str
    | A.PrimTyp(A.Bool) -> bool_str
    | A.PrimTyp(A.Real) -> real_str
    | A.String          -> string_str
  in
  let lookup n map = try StringMap.find n map
                     with Not_found -> raise (Failure "ERROR: asn not found.")
  in

  let build_statements (builder, var_map) stmt = 
    let rec expr builder var_map (typ, e) = match e with
        SLit i -> L.const_int i32_t i
      | SCharLit c -> L.const_int i8_t (Char.code c)
      | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | SRealLit r -> L.const_float f32_t r
      | SSetLit sl -> let hset_ptr = L.build_call init_hset_func [| |] "init_hset" builder 
          in add_list_vals sl (fst (List.hd sl)) hset_ptr  
      | SId s -> L.build_load (lookup s var_map) s builder
      | SStringLit s -> L.build_global_stringptr s ".str" builder
      | SInterStringLit(sl, el) -> 
        let frmt = String.concat "%s" sl in
        let llfrmt = L.build_global_stringptr frmt ".str" builder in
        let expr_to_llstr (typ, se) =
          let llval = expr builder var_map (typ, se) in
          let val_addr = L.build_alloca (ltype_of_typ typ) "" builder in
          let _ = L.build_store llval val_addr builder in
          let bitcast = L.build_bitcast val_addr ptr_t "bitcast" builder in
          L.build_call string_of_func [| bitcast ; strtype_of_typ typ |] "string_of" builder
        in
        let str_addrs = List.map expr_to_llstr el in
        let str_num = L.const_int i32_t (List.length str_addrs) in
        let params = Array.of_list (llfrmt::str_num::str_addrs) in
        let fcall = L.build_call string_interpolation_func params "temp" builder in
        L.build_call free_args_func (Array.of_list (str_num::str_addrs)) "" builder ; fcall
      | SFuncCall ("print", e) -> L.build_call printf_func [| str_format_str ; (expr builder var_map e) |] "printf" builder 
      | SFuncCall ("print_set", e) -> L.build_call print_hset_func [| (expr builder var_map e) |] "" builder
      | SBinop (e1, op, e2) -> 
        let (t, _) = e1
        and e1' = expr builder var_map e1
        and e2' = expr builder var_map e2 in
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
        | _         -> to_imp "Binop not yet implemented"
        ) e1' e2' "tmp" builder
        else if t = A.Set(A.PrimTyp(A.Int)) then match op with
          A.Add     -> L.build_call hset_union_func [| e1' ; e2' ; int_str |] "hset_union" builder 
        | A.Sub     -> L.build_call hset_diff_func [| e1' ; e2' ; int_str |] "hset_diff" builder 
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
        | _         -> to_imp "Binop not yet implemented"
        ) e1' e2' "tmp" builder
      | SUniop(op, e) ->
        let (t, _) = e and e' = expr builder var_map e in
        (match op with
          A.Neg when t = A.PrimTyp(A.Real) -> L.build_fneg 
        | A.Neg                            -> L.build_neg
        ) e' "tmp" builder
      | STupleLit(sel) ->
        let llvals = List.map (expr builder var_map) sel in
        let tup_addr = L.build_alloca (L.element_type (ltype_of_typ typ)) "temp" builder in
        let store_val i v = 
          let gep_ptr = L.build_struct_gep tup_addr i "" builder in
          ignore(L.build_store v gep_ptr builder)
        in
        List.iteri store_val llvals; tup_addr
      | SAggAccessor(e1, e2) -> (
        let llptr = expr builder var_map e1 in
        match fst e1 with
        | A.Tuple(_) -> (
          match snd e2 with
          | SLit(i) -> 
            let gep_ptr = L.build_struct_gep llptr i "" builder in
            L.build_load gep_ptr "" builder
          | _ -> raise(Failure "semant shouldn't have allowed non-literal int index for tuple") 
          )
        | A.Array(_) -> to_imp "Array indexing"
        )
      | _ -> to_imp "expression builder" (* TODO: implemnet variable reference *)
    and add_list_vals (slist: sexpr list) t hset_ptr = match slist with
      | [] -> raise (Failure "empty list added to set") 
      | [ se ] -> let val_addr = L.build_alloca (ltype_of_typ t) "temp" builder in
          let _ = L.build_store (expr builder var_map se) val_addr builder in
          let bitcast = L.build_bitcast val_addr str_t "bitcast" builder in 
          L.build_call add_val_func [| bitcast; (strtype_of_typ t); hset_ptr |] "add_val" builder 
      | head :: tail -> 
          let new_hset_ptr = 
            let val_addr = L.build_alloca (ltype_of_typ t) "temp" builder in
            let _ = L.build_store (expr builder var_map head) val_addr builder in
            let bitcast = L.build_bitcast val_addr str_t "bitcast" builder in 
            L.build_call add_val_func [| bitcast; (strtype_of_typ t); hset_ptr |] "add_val" builder 
          in add_list_vals tail t new_hset_ptr 
    and del_list_vals (slist: sexpr list) t hset_ptr = match slist with
      | [] -> raise (Failure "empty list subtracted from set") 
      | [ se ] -> let val_addr = L.build_alloca (ltype_of_typ t) "temp" builder in
          let _ = L.build_store (expr builder var_map se) val_addr builder in
          let bitcast = L.build_bitcast val_addr str_t "bitcast" builder in 
          L.build_call del_val_func [| bitcast; (strtype_of_typ t); hset_ptr |] "del_val" builder 
      | head :: tail -> 
          let new_hset_ptr = 
            let val_addr = L.build_alloca (ltype_of_typ t) "temp" builder in
            let _ = L.build_store (expr builder var_map head) val_addr builder in
            let bitcast = L.build_bitcast val_addr str_t "bitcast" builder in 
            L.build_call del_val_func [| bitcast; (strtype_of_typ t); hset_ptr |] "del_val" builder 
          in add_list_vals tail t new_hset_ptr 
    in 

    let add_terminal builder f = match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (f builder) in

    (* match stmt with *)
    let rec stmt_builder (builder, var_map) = function 
          SExpr e -> ignore(expr builder var_map e); (builder, var_map)
        (* Handle a declaration *)
        | SDecl (n, t) -> let addr = L.build_alloca (ltype_of_typ t) n builder
            in (builder, StringMap.add n addr var_map) (* TODO DONT IGNORE THIS *)

        | SAsn (n, sexpr) -> let addr = lookup n var_map
            in let e' = expr builder var_map sexpr
            in let _ = L.build_store e' addr builder 
            in (builder, var_map) (* TODO: should this really be ignored? *)
        | SIf (predicate, then_stmt, else_stmt) ->
            let bool_val = expr builder var_map predicate in
            let merge_bb = L.append_block context "merge" the_function in
            let branch_instr = L.build_br merge_bb in

            (* then basic block *)
            let then_bb = L.append_block context "then" the_function in
            let then_builder, var_map = List.fold_left stmt_builder (L.builder_at_end context then_bb, var_map) then_stmt in
            let () = add_terminal then_builder branch_instr in

            (* else basic block *)
            let else_bb = L.append_block context "else" the_function in
            let else_builder, var_map = List.fold_left stmt_builder (L.builder_at_end context else_bb, var_map) else_stmt in
            let () = add_terminal else_builder branch_instr in

            let _ = L.build_cond_br bool_val then_bb else_bb builder in
            (* Move to the merge block for further instruction building *)
            (L.builder_at_end context merge_bb, var_map)
        | SWhile (predicate, body) ->
            let pred_bb = L.append_block context "while" the_function in
            let _ = L.build_br pred_bb builder in

            let body_bb = L.append_block context "while_body" the_function in
            let while_builder, var_map = List.fold_left stmt_builder (L.builder_at_end context body_bb, var_map) body in
            let () = add_terminal while_builder (L.build_br pred_bb) in

            let pred_builder = L.builder_at_end context pred_bb in
            let bool_val = expr pred_builder var_map predicate in

            let merge_bb = L.append_block context "merge" the_function in
            let _ = L.build_cond_br bool_val body_bb merge_bb pred_builder in
            (L.builder_at_end context merge_bb, var_map)
        | _ -> to_imp "Statement not yet handled"

    in stmt_builder (builder, var_map) stmt

  in let builder = fst (List.fold_left build_statements (builder, StringMap.empty) stmt_list) in
  ignore(L.build_ret (L.const_int i32_t 0) builder);
  the_module
