(* Semantic checking *)

open Ast
open Sast
open Pprinting

module StringMap = Map.Make(String)

type scope = {symb: Ast.typ StringMap.t; parent: scope option}

(* 
 * Should check
 * 1) types match
 * 2) Variables are declared before usage
 * 3) Variables are in scope
 *)

(* stmts: stmt list *)
let check stmts = 
  let rec type_of_identifier var scope =
    try StringMap.find var scope.symb
    with Not_found -> 
    match scope.parent with
    | None -> raise (Failure ("undeclared identifier " ^ var))
    | Some paren_sc -> type_of_identifier var paren_sc
  in
  let add_to_scope var typ scope = 
    {scope with symb = StringMap.add var typ scope.symb} 
  in
  let check_asn left_t right_t err = match right_t with
    | Set(PrimTyp(Char)) | Set(PrimTyp(Bool)) | Set(PrimTyp(Real))
          -> if left_t = PrimTyp(Int) then ignore(left_t)
    | _ -> if left_t = right_t then ignore(left_t) else raise (Failure err)
  in 
  let array_element_type arr_t = match arr_t with
    Array(x) -> x
    | _ -> raise (Failure "Not an array type")
  in
  (* Return a semantically-checked expression, i.e., with a type *)
  (* TODO: correct expr *)
  let rec expr e scope = match e with
    | Id s  -> (type_of_identifier s scope, SId s)
    | Binop (e1, op, e2) ->
      let (t1, e1') = expr e1 scope
      and (t2, e2') = expr e2 scope in
      (* All binary operators require operands of the same type *)
      let same = t1 = t2 in
      (* Determine expression type based on operator and operand types *)
      let ty = match op with
        Add | Sub | Mul | Div when same && t1 = PrimTyp(Int)  -> PrimTyp(Int)
      | Add | Sub | Mul | Div when same && t1 = PrimTyp(Real) -> PrimTyp(Real)
      | Add | Sub | Mul | Div when same && t1 = Set(PrimTyp(Int)) -> Set(PrimTyp(Int))
      | Equal | Neq           when same              -> PrimTyp(Bool)
      | Less | Leq | Greater | Geq
                 when same && (t1 = PrimTyp(Int) || t1 = PrimTyp(Real)) -> PrimTyp(Bool)
      | And | Or when same && t1 = PrimTyp(Bool) -> PrimTyp(Bool)
      | Member -> ( match t2 with 
        | Set(set_t) when set_t = t1 -> PrimTyp(Bool)
        | _ -> raise(Failure("membership operand needs to be set type")) )
      | _ -> raise (Failure ("illegal binary operator")) (* TODO: full error statement *)
      in (ty, SBinop((t1, e1'), op, ((t2, e2')))) 
    | Uniop (op, e) ->
      let (t, e') = expr e scope in
      let ty = match op with
        Neg when t = PrimTyp(Int) || t = PrimTyp(Real) || t = PrimTyp(Bool) -> t
      | _ -> raise (Failure ("illegal unary operator"))
      in (ty, SUniop(op, (t, e')))
    | Lit l -> (PrimTyp(Int), SLit l)
    | RealLit s -> (PrimTyp(Real), SRealLit s)
    | BoolLit b -> (PrimTyp(Bool), SBoolLit b)
    | CharLit c -> (PrimTyp(Char), SCharLit c)
    | StringLit s -> (String, SStringLit s)
    | InterStringLit (sl, el) -> 
      (String, SInterStringLit (sl, List.map (fun ex -> expr ex scope) el))
    | TupleLit t -> 
      let sexpr_list = List.map (fun ex -> expr ex scope) t in
      ( Tuple (List.map fst sexpr_list), 
        STupleLit (sexpr_list) )
    | SetLit l -> 
      let set_t = match l with
        | [] -> PrimTyp(Int) (* this is bad, should look into type for empty collection *)
        | h::t -> fst (expr h scope)
      in let sexpr_list = List.map (fun ex -> expr ex scope) l
      in let is_valid = List.fold_left (fun b se -> b && (set_t = (fst se))) true sexpr_list
      in (
        match is_valid with
        | false -> raise (Failure ("all elements of set must have type " ^ (string_of_typ set_t)))
        | true -> (Set(set_t), SSetLit (sexpr_list))
      )
    (* | SetBuilder(epot, st, ex) ->
      match st with
      | Iter(sl, ex)
      TODO: make iter into expression parser-wise and choose between iter and member
       *)
    | ArrayLit l ->
      let arr_t = match l with
        | [] -> PrimTyp(Int) (* this is bad, should look into type for empty collection *)
        | h::t -> fst (expr h scope)
      in let sexpr_list = List.map (fun ex -> expr ex scope) l
      in let is_valid = List.fold_left (fun b se -> b && (arr_t = (fst se))) true sexpr_list
      in (
        match is_valid with
        | false -> raise (Failure ("all elements of array must have type " ^ (string_of_typ arr_t)))
        | true -> (Array(arr_t), SArrayLit (sexpr_list))
      )
    | ArrayGet(l, i) ->
      let idx = expr i scope in
      let _ = match fst idx with
        | PrimTyp(Int) -> PrimTyp(Int)
        | _ -> raise (Failure ("index of array must be an integer"))
      in
      let arr_t = type_of_identifier l scope in
      (array_element_type arr_t, SArrayGet(l, idx))
    | ArrayAt(l, i, e) ->
      let idx = expr i scope in
      let _ = match fst idx with
        | PrimTyp(Int) -> PrimTyp(Int)
        | _ -> raise (Failure ("index of array must be an integer"))
      in
      let e' = expr e scope in
      let (new_ty, _) = e' in
      let arr_t = array_element_type (type_of_identifier l scope) in
      let _ = check_asn arr_t new_ty "New element needs to have same type as existing array elements" in
      (arr_t, SArrayAt(l, idx, e'))
    | ArrayRange(e1, i, e2) -> 
      let e1' = expr e1 scope in
      let e2' = expr e2 scope in
      let inc = match i with
        | Some x -> Some (expr x scope)
        | None -> None
      in
      let (e1_ty, _) = e1' in
      let (e2_ty, _) = e2' in
      let _ = check_asn e1_ty e2_ty "Array range elements must be the same type" in
      let arr_t = fst e1' in
      (Array(arr_t), SArrayRange(e1', inc, e2'))
    | FuncDefNamed(f, al, sl) -> ( (* f, al, sl = function name, expr list ne, statement list *)
      let func_t = type_of_identifier f scope in
      match func_t with
      | Func(in_t, out_t) ->
        let arg_typs = match in_t with
          | Tuple(l) -> l
          | t -> [t]
        in let arg_decl = 
          try List.map2 (fun a t -> Decl(a, t)) al arg_typs
          with Invalid_argument(_) -> raise(Failure("number of arguments mismatch"))
        in let local_scope = {symb = StringMap.empty; parent = Some scope}
        in let arg_sdecl = append_sstmt local_scope arg_decl
        in let local_scope = check_stmt arg_decl local_scope
        in let _ = check_stmt sl local_scope
        in let ssl = append_sstmt local_scope sl
        in let _ = match List.hd (List.rev ssl) with
          | SExpr(typ, sx) -> 
            let err = "output type mismatch " ^ string_of_typ out_t ^ " with " ^ string_of_typ typ
            in check_asn out_t typ err
          | _ -> raise(Failure("parser shouldn't have returned function not ending with expr"))
        in (func_t, SFuncDef (arg_sdecl, ssl))
      | _ -> raise (Failure("non-function type stored")) )
    | FuncCall(var, e) -> (
      let typ = type_of_identifier var scope 
      and sexpr = expr e scope (* tuple *)
      in match typ with
      | Func(in_typ, out_typ) as ex ->
        let e_typ = fst sexpr in
        let err = "illegal assignment " ^ string_of_typ in_typ ^ " = " ^ 
            string_of_typ e_typ ^ " in " ^ string_of_typ ex
        in let _ = check_asn in_typ e_typ err
        in (out_typ, SFuncCall(var, sexpr))
      | _ -> raise (Failure ("non-function type stored")) )
    | _ -> raise (Failure ("not matched"))
  
  and check_stmt to_check symbols = 
    let check_bool_expr e = 
      let (t', e') = expr e symbols
      and err = "expected Boolean expression in " ^ string_of_expr e
      in if t' != PrimTyp(Bool) then raise (Failure err) else (t', e') 
    in
    match to_check with
    | [] -> symbols
    | stmt :: tail -> match stmt with
      | Decl (var, t) -> check_stmt tail (add_to_scope var t symbols)
      | Asn(var, e) as st ->
        let left_t = type_of_identifier var symbols
        and (right_t, e') = expr e symbols in
        let err = "illegal assignment " ^ string_of_typ left_t ^ " = " ^ 
          string_of_typ right_t ^ " in " ^ string_of_stmt st
        in let _ = check_asn left_t right_t err 
        in check_stmt tail symbols
      | AsnDecl(var, e) -> 
        let (et, se) = expr e symbols in
        check_stmt tail (add_to_scope var et symbols)
      | Expr e -> check_stmt tail symbols  
      | If(p, b1, b2) -> check_bool_expr p; check_stmt b1 symbols; check_stmt b2 symbols
      | While(p, s) -> check_bool_expr p; check_stmt s symbols
      | For(n, p, s) ->
        let t = PrimTyp(Int) in
        let map = add_to_scope n t symbols in
        check_stmt s map(* TODO need to check type of p and that n is var *)

  (* recursively gather sstmt list *)
  and append_sstmt symbols = function
    | h :: t -> (
      match h with
      | Expr e -> (SExpr (expr e symbols)) :: (append_sstmt symbols t)
      | Asn(var, e) ->
        (SAsn (var, expr e symbols)) :: (append_sstmt symbols t)
      | Decl(var, tp) ->
        let symbols' = add_to_scope var tp symbols in
        (SDecl(var, tp)) :: (append_sstmt symbols' t)
      | AsnDecl(var, e) ->
        let (tp, se) = expr e symbols in
        let symbols' = add_to_scope var tp symbols in
        (SDecl(var, tp)) :: (SAsn (var, (tp, se))) :: (append_sstmt symbols' t)
      | If(p, b1, b2) -> 
        let (tp, se) = expr p symbols in
        SIf((tp, se), append_sstmt symbols b1, append_sstmt symbols b2) :: (append_sstmt symbols t)
      | While(p, s) -> 
        let (tp, se) = expr p symbols in
        SWhile((tp, se), append_sstmt symbols s) :: (append_sstmt symbols t)
      | For(n, p, s) ->
         let (tp, se) = expr p symbols in
         SFor(n, (tp, se), append_sstmt symbols s) :: (append_sstmt symbols t)
    )
    | [] -> []
  in
  let symbols_init = StringMap.add "print" (Func(PrimTyp(Int), PrimTyp(Int))) StringMap.empty in
  let symbols_init = StringMap.add "print_string" (Func(String, PrimTyp(Int))) symbols_init in
  let symbols_init = StringMap.add "print_set" (Func(Set(PrimTyp(Int)), PrimTyp(Int))) symbols_init in
  let g_scope = {symb = symbols_init; parent = None} in 
  let symbols = check_stmt stmts g_scope
  in append_sstmt symbols stmts

