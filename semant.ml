(* Semantic checking *)

open Ast
open Sast
open Pprinting

module StringMap = Map.Make(String)

(*
 * WHERE WE LEFT OFF
 * -----------------
 * Our semantic checker `semant.ml` is currently heavily based on the `microc` semantic checker.
 *
 * Our pipeline thus works like this:
     * read list of globals into a StringMap
     * build (variable) symbol table
     * check type match for stmts, pattern matching stmt types Expr, Asn, Decl
 *
 * WHAT TO DO NEXT
 * ---------------
 * microc considers `bind` a type of its own, but heckell considers the equivalent (Decl) of type stmt.
 * So we need to match only on stmt-Decls, and build the symbol table accordingly.
 * Then we may perform semantic checking, evaluating `Asn`s along the way.
 *
 * Our first test case should look something like:
     * let x:int;
     * x = 3;
     * let y:int;
     * y = 3.0;
     * and observe the corresponding semantic error.
 * Then we can continue with evaluating the types of stmt-Exprs, etc.
 *)


(* 
 * Should check
 * 1) types match
 * 2) Variables are declared before usage
 * 3) Variables are in scope
 *)

(* stmts: stmt list *)
let check stmts = 

  let type_of_identifier var map =
    try StringMap.find var map
    with Not_found -> raise (Failure ("undeclared identifier " ^ var))
  in
  (* Return a semantically-checked expression, i.e., with a type *)
  (* TODO: correct expr *)
  let rec expr e map = match e with
    | Id s  -> (type_of_identifier s map, SId s)
    | Lit l -> (PrimTyp(Int), SLit l)
    | RealLit s -> (PrimTyp(Real), SRealLit s)
    | BoolLit b -> (PrimTyp(Bool), SBoolLit b)
    | TupleLit t -> 
      let sexpr_list = List.map (fun ex -> expr ex map) t in
      ( Tuple (List.map fst sexpr_list), 
        STupleLit (sexpr_list) )
    | SetLit l -> 
      (* let set_t = 
        match l with
        | [] -> PrimTyp(Int) this is bad, should look into what type an empty set it
        | h::t -> fst (expr h)
      in 
      let sexpr_list = List.map expr l in *)
      let set_t = match l with
        | [] -> PrimTyp(Int) (* this is bad, should look into type for empty collection *)
        | h::t -> fst (expr h map)
      in let sexpr_list = List.map (fun ex -> expr ex map) l
      in let is_valid = true (* List.fold_left (fun b se -> b and (set_t = fst se)) true sexpr_list *)
      in (
        match is_valid with
        | false -> raise (Failure ("all elements of set must have type " ^ (string_of_typ set_t)))
        | true -> (Set(set_t), SSetLit (sexpr_list))
      )
    | ArrayLit l ->
      let arr_t = match l with
        | [] -> PrimTyp(Int) (* this is bad, should look into type for empty collection *)
        | h::t -> fst (expr h map)
      in let sexpr_list = List.map (fun ex -> expr ex map) l
      in let is_valid = true (* List.fold_left (fun b se -> b and (arr_t = fst se)) true sexpr_list *)
      in (
        match is_valid with
        | false -> raise (Failure ("all elements of array must have type " ^ (string_of_typ arr_t)))
        | true -> (Set(arr_t), SArrayLit (sexpr_list))
      )
(*     | Binop
    | Uniop
    | ArrayRange
    | SetBuilder
    | FuncDef
    | FuncCall *)
  in
  let check_asn left_t right_t err =
    if left_t = right_t then left_t else raise (Failure err)
  in 
  let rec check_stmt to_check symbols = 
    match to_check with
    | [] -> symbols
    | stmt :: tail -> match stmt with
      | Decl (var, t) -> check_stmt tail (StringMap.add var t symbols)
      | Asn(var, e) as st ->
          let left_t = type_of_identifier var symbols
          and (right_t, e') = expr e symbols in
          let err = "illegal assignment " ^ string_of_typ left_t ^ " = " ^ 
            string_of_typ right_t ^ " in " ^ string_of_stmt st
          in let _ = check_asn left_t right_t err 
          in check_stmt tail symbols
      | Expr e -> check_stmt tail symbols  
  in
  let symbols = check_stmt stmts StringMap.empty

  (* gather sstmt list *)
  in 
  let append_sstmt stmt =
    match stmt with
    Expr e -> SExpr (expr e symbols)
    | Asn(var, e) -> 
        SAsn(var, expr e symbols)
    | Decl(var, t) -> SDecl(var, t)
  in 
  List.map append_sstmt stmts

