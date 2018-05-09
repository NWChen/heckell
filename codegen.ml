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
  let i1_t       = L.i1_type        context in
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
      L.var_arg_function_type void [| str_t |] in
  let destroy_hset_func : L.llvalue =
      L.declare_function "destroy_hset" destroy_hset_t the_module in

  let print_hset_t : L.lltype =
      L.var_arg_function_type void [| str_t |] in
  let print_hset_func : L.llvalue =
      L.declare_function "print_hset" print_hset_t the_module in

  let hset_len_t : L.lltype =
      L.var_arg_function_type i32_t [| str_t |] in
  let hset_len_func : L.llvalue =
      L.declare_function "hset_len" hset_len_t the_module in

  let get_next_t : L.lltype =
      L.var_arg_function_type str_t [| str_t |] in
  let get_next_func : L.llvalue =
      L.declare_function "get_next" get_next_t the_module in

  let get_val_t : L.lltype =
      L.var_arg_function_type str_t [| str_t |] in
  let get_val_func : L.llvalue =
      L.declare_function "get_val" get_val_t the_module in

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
                     with Not_found -> raise (Failure ("ERROR: asn " ^ n ^ " not found."))
  in

  let get_int_or_float e = match e with
      (_, SLit i) -> i
    | _ -> raise (Failure "internal error: semant should have rejected, only int or real allowed")
  in

  let rec build_list s inc e = 
    let (t, _) = s in
    let x = (get_int_or_float s) in
    if inc > 0 then
      if x <= (get_int_or_float e) then let x' = (t, (SLit (x + inc))) in s::(build_list x' inc e) else []
    else
      if x >= (get_int_or_float e) then let x' = (t, (SLit (x + inc))) in s::(build_list x' inc e) else []
  in

  let build_statements (builder, var_map) stmt = 
    let rec expr builder var_map (out_typ, e) = match e with (* used to be (_, e) but we need that `out_typ` for SFuncCall *)
        SLit i -> L.const_int i32_t i
      | SCharLit c -> L.const_int i8_t (Char.code c)
      | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | SRealLit r -> L.const_float f32_t r
      | SSetLit sl -> let hset_ptr = L.build_call init_hset_func [| |] "init_hset" builder 
          in add_list_vals sl (fst (List.hd sl)) hset_ptr  
      | SId s -> L.build_load (lookup s var_map) s builder
      | SStringLit s -> L.build_global_stringptr s ".str" builder
      | SFuncCall ("print", e) -> L.build_call printf_func [| int_format_str ; (expr builder var_map e) |] "printf" builder 
      | SFuncCall ("print_string", e) -> L.build_call printf_func [| str_format_str ; (expr builder var_map e) |] "printf" builder
      | SFuncCall (s, e) ->
        let result = s ^ "_result" and f = StringMap.find s var_map in (* f: llvalue representing function <s> *)
        L.build_call f (match e with
          | (A.Tuple(actual_typs), STupleLit(el)) -> Array.of_list (List.map (fun arg -> expr builder var_map arg) el)  (* TODO revise el evaluation *)
          | x -> [| expr builder var_map x |]
        ) result builder

        (* let result = s ^ "_result" and f = L.define_function s (ltype_of_typ out_typ) the_module in
        L.build_call f (match e with *)
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
      | SArrayLit(x) -> 
        let (arr_t, _) = List.hd x in
        let addr = L.build_array_alloca (ltype_of_typ arr_t) (L.const_int i32_t (List.length x)) "tmp" builder in
        let _ = initialize_arr addr x var_map in
        addr
      | SArrayGet(l, i) ->
        let addr = lookup l var_map in
        let idx = expr builder var_map i in
        let pointer = L.build_gep addr [|idx|] "tmp" builder in
        L.build_load pointer "tmp" builder
      | SArrayAt(l, i, e) -> 
        let e' = expr builder var_map e in
        let idx = expr builder var_map i in
        let addr = lookup l var_map in
        let pointer = L.build_gep addr [|idx|] "tmp" builder in
        L.build_store e' pointer builder
      | SArrayRange(e1, i, e2) ->
        let (arr_t, _) = e1 in
        let lis = match i with
          | Some x ->
              if (get_int_or_float x) == (get_int_or_float e1) then
                raise (Failure "Second argument of array range must not be the same as the first value")
              else 
                build_list e1 ((get_int_or_float x) - (get_int_or_float e1)) e2
          | None -> build_list e1 1 e2
        in
        let addr = L.build_array_alloca (ltype_of_typ arr_t) (L.const_int i32_t (List.length lis)) "tmp" builder in
        let _ = initialize_arr addr lis var_map in
        addr
      | _ -> to_imp "Expression not yet handled"
    
    and map_build x o addr =
      let x' = expr builder var_map x in
      let arr_ptr = L.build_gep addr [| L.const_int i32_t o |]
          "tmp" builder in
      let _ = L.build_store x' arr_ptr builder
      in o + 1
    
    and initialize_arr addr el var_map = List.fold_left (fun o e -> map_build e o addr) 0 el
      
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
        | SExpr e -> let _ = expr builder var_map e in (builder, var_map)
        (* Handle a declaration *)
        | SDecl (n, t) ->
            let allocate n' t' b = L.build_alloca (ltype_of_typ t') n' b in (* t' != t passed to SDecl; `b` is a builder *)
            let var_map = (match t with
              (* primitive type declaration *)
              | A.PrimTyp(_) -> 
                  StringMap.add n (allocate n t builder) var_map (* L.build_alloca (ltype_of_typ t) n builder in*)
              | A.Array(t) -> (* A bit of a hack here. We initialize the array to have size 1 when declared and adjust the size later when the array is assigned. *)
                let addr = L.build_array_alloca (ltype_of_typ t) (L.const_int i32_t 1) n builder in
                StringMap.add n addr var_map
              | A.Func(in_t, out_t) -> (* Function declaration. *)
                let name_formals formals = List.mapi (fun i _ -> n ^ (string_of_int i)) formals in (* we only know their type so far - thus formals are temporarily named n0, n1, ... where n = function name *) (* TODO check these temp names in Sasn. *)
                let formal_typs = match in_t with
                  | A.PrimTyp(_) -> [in_t]
                  | A.Tuple(l) -> l
                in
                let formal_typs = Array.of_list (List.map (fun t -> ltype_of_typ t) formal_typs) in
                let func_typ = L.function_type (ltype_of_typ out_t) formal_typs in
                let func_def = L.define_function n func_typ the_module in
                let this_function = L.builder_at_end context (L.entry_block func_def) in
                let var_map = (match in_t with
                  | A.PrimTyp(_) -> let [formal] = name_formals [in_t] in
                    StringMap.add formal (allocate formal in_t this_function) var_map
                  | A.Tuple(l) -> let formals = name_formals l in
                    List.fold_left2 (fun m n' t' -> StringMap.add n' (allocate n' t' this_function) m) var_map formals l) in
                StringMap.add n func_def var_map
              | _ -> let _ = L.build_alloca (ltype_of_typ t) n builder in
                var_map
            ) in (builder, var_map)

        | SAsn (n, sexpr) -> let var_map = (match sexpr with

            (* Function definition *)
            | (A.Func(in_t, out_t), SFuncDef (args, stmts)) ->

                (* Build formals, declaration, etc. *)
                let formals = List.mapi (fun i _ -> n ^ (string_of_int i)) args in
                let formals' = List.map (fun arg -> match arg with (* Formals, now attached to names, specified in function assignment(definition). *)
                  | SDecl (n, _) -> n
                  | _ -> raise (Failure ("Improperly specified argument (expected declaration)."))
                ) args in
                let formal_instrs = List.map (fun f -> StringMap.find f var_map) formals in
                let var_map = List.fold_left2 (fun m f f' -> (* Replace temporary formal name bindings in `var_map` with new names. *)
                  let instr = StringMap.find f m in
                  (StringMap.add f' instr (StringMap.remove f m))
                ) var_map formals formals' in
                let formal_instrs' = List.map (fun f' -> StringMap.find f' var_map) formals' in
                let _ = List.iter2 (fun f f' -> L.replace_all_uses_with f f') formal_instrs formal_instrs' in (* Replace temporary (<n>0, <n>1, ...) formal names in LLVM with new (user-specified) names. *)
                (* Generate LLVM in the basic block entered by function <n> *)
                let this_function = StringMap.find n var_map in
                let builder = L.builder_at_end context (L.entry_block this_function) in
                let _ = List.map2 (fun (SDecl (n, t)) p ->
                  L.build_store p (StringMap.find n var_map) builder
                ) args (Array.to_list (L.params this_function)) in
                let (builder, _) = List.fold_left stmt_builder (builder, var_map) stmts in

                (* Return latest-evaluated top-level (no children, e.g. in `If`) `expr` *)
                let rec return_expr revd_stmts = match revd_stmts with
                  | [] -> (A.PrimTyp(A.Int), SLit(0)) (* `heckell` returns `0` when no expression inside a function can be evaluated (nothing to return). *)
                  | SExpr(e') :: _ -> e'
                  | _ :: tl -> return_expr tl
                in  
                let e' = expr builder var_map (return_expr (List.rev stmts)) in
                let return_instr = L.build_ret e' in
                let _ = add_terminal builder return_instr in
                  var_map

            | (_, e) -> let addr = lookup n var_map in
                let e' = expr builder var_map sexpr in
                match e with
                  | SArrayRange(e1, i, e2) -> StringMap.add n e' var_map
                  | SArrayLit(x) -> StringMap.add n e' var_map
                  | _ -> let _ = L.build_store e' addr builder in var_map
            ) in (builder, var_map)

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
        | SFor (n, a, body) -> 
            let base_addr = expr builder var_map a in
            let len = match (snd a) with
                SArrayLit(x) -> List.length x
              | SArrayRange(e1, i, e2) ->
                (match i with
                    Some x -> List.length (build_list e1 ((get_int_or_float x) - (get_int_or_float e1)) e2)
                  | None -> List.length (build_list e1 1 e2))
              | SSetLit(x) -> List.length x
              | _ -> raise (Failure "incorrect type")
            in
            let var = L.build_alloca i32_t n builder in (* hardcoded type *)

            match (snd a) with
              | SArrayLit(_) | SArrayRange(_) -> 
                  let rec for_body_arr i len = 
                    if i < len then
                      let offset = L.const_int i32_t i in
                      let arr_ptr = L.build_gep base_addr [| offset |] "val_ptr" builder in
                      let arr_val = L.build_load arr_ptr "new_val" builder in

                      let _ = L.build_store arr_val var builder in

                      let new_var_map = StringMap.add n var var_map in
                      List.fold_left stmt_builder (builder, new_var_map) body;
                      for_body_arr (i + 1) len
                    else
                      (builder, var_map)
                  in
                  for_body_arr 0 len
              | SSetLit(_) -> 
                  let rec for_body_sets i len curr =
                    if i < len then
                      let set_val_ptr = L.build_call get_val_func [| curr |] "get_val" builder in
                      let bitcast = L.build_bitcast set_val_ptr intp_t "bitcast" builder in
                      let set_val = L.build_load bitcast "new_val" builder in

                      let _ = L.build_store set_val var builder in

                      let new_var_map = StringMap.add n var var_map in
                      List.fold_left stmt_builder (builder, new_var_map) body;

                      let next = L.build_call get_next_func [| curr |] "get_next" builder in
                      for_body_sets (i + 1) len next

                    else
                      (builder, var_map)
                  in
                  for_body_sets 0 len base_addr
        | _ -> to_imp "Statement not yet handled"

    in stmt_builder (builder, var_map) stmt

  in let builder = fst (List.fold_left build_statements (builder, StringMap.empty) stmt_list) in
  ignore(L.build_ret (L.const_int i32_t 0) builder);
  the_module
