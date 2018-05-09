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
  let i1_t       = L.i1_type        context in
  (* Create an LLVM module -- this is a "container" into which we'll 
     generate actual code *)
  let the_module = L.create_module context "Heckell" in

  (* Convert Heckell types to LLVM types *)
  let ltype_of_typ = function
      A.PrimTyp(A.Int) -> i32_t
    | A.PrimTyp(A.Bool)  -> i1_t
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
                     with Not_found -> raise (Failure ("ERROR: asn " ^ n ^ " not found."))
  in
  let build_statements (builder, var_map) stmt = 
    let rec expr builder var_map (out_typ, e) = match e with (* used to be (_, e) but we need that `out_typ` for SFuncCall *)
        SLit i -> L.const_int i32_t i
      | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
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
      | _ -> to_imp "expression builder" (* TODO: implemnet variable reference *)
    in 

    let add_terminal builder f = match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (f builder) in

    (* match stmt with *)
    let rec stmt_builder (builder, var_map) = function 
          SExpr e -> ignore(expr builder var_map e); (builder, var_map)
        (* Handle a declaration *)

        | SDecl (n, t) -> 
            let allocate n' t' b = L.build_alloca (ltype_of_typ t') n' b in (* t' != t passed to SDecl; `b` is a builder *)
            let var_map = (match t with
              (* primitive type declaration *)
              | A.PrimTyp(_) -> 
                  StringMap.add n (allocate n t builder) var_map (* L.build_alloca (ltype_of_typ t) n builder in*)

              (* function declartion *)
              | A.Func(in_t, out_t) ->
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
            ) in (builder, var_map)

        | SAsn (n, sexpr) -> let _ = (match sexpr with

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
                add_terminal builder return_instr (* TODO fix returns *)
            | _ -> let addr = lookup n var_map in
                let e' = expr builder var_map sexpr in
                let _ = L.build_store e' addr builder in ()
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
        | _ -> to_imp "Statement not yet handled"

    in stmt_builder (builder, var_map) stmt

  in let builder = fst (List.fold_left build_statements (builder, StringMap.empty) stmt_list) in
  ignore(L.build_ret (L.const_int i32_t 0) builder);
  the_module
