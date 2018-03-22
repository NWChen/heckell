(* Semantic checking *)

open Ast
open Sast

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

(* stmts: stmt list *)
let check stmts = 

    let type_of_identifier var symbols =
      try StringMap.find var symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ var))
    in
    (* Return a semantically-checked expression, i.e., with a type *)
    (* TODO: correct expr *)
    let rec expr e = match e with
        Lit l -> (Int, SLit l)
        | Id s       -> (type_of_identifier s symbols, SId s)
    in
    let check_asn left_t right_t err =
        if left_t = right_t then left_t else raise (Failure err)
    in 
    let rec check_stmt to_check symbols = 
      match to_check with
      [] -> symbols
      | stmt :: tail -> match stmt with
        Decl (var, t) -> StringMap.add var t symbols
        | Asn (var, e) as ex ->
          let left_t = type_of_identifier var symbols
          and (right_t, e') = expr e in
          let err = "illegal assignment " in (* TODO rest of error message *)
          in let _ = check_asn left_t right_t err in symbols
        | Expr e -> symbols; (* TODO review this *) 
        check_stmt tail symbols
    in
    let symbols = check_stmt stmts StringMap.empty

    (* gather sstmt list *)
    in 
    let append_sstmt = function
        Expr e -> SExpr (expr e)
        | Asn(var, e) as ex -> SAsn(var, (StringMap.find var symbols, expr e)
        | Decl(t, var) -> SDecl(t, var)
    in 
    List.fold_left append_sstmt [] stmts 

