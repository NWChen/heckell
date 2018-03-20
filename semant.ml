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
      try StringMap.find var map
      with Not_found -> raise (Failure ("undeclared identifier " ^ var))
    in
    (* Return a semantically-checked expression, i.e., with a type *)
    (* TODO: correct expr *)
    let rec expr = function
        Lit l -> (Int, SLit l)
        | Id s       -> (type_of_identifier s symbols, SId s)

    in
    let check_asn checked asn = 
        (* ... *)

    
    let check_expr checked expr = 
        (* ... *)
    in
    let check_stmt stmt symbols =
        let invalid_err = "invalid statement" 
        in match stmt with
        Expr e -> (SExpr (expr e), symbols)
        | Asn(var, e) as ex ->
            let lt = type_of_identifier var 
            and (rt, e') = expr e in
            let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^ string_of_typ rt ^ " in " ^ string_of_expr ex in
            check_assign lt rt err, SAsn(var, (rt, e'))
        | Decl(var, t) -> 
            StringMap.add var t symbols (* TODO: check mutating symbols map *)
            in (SDecl(var, t), symbols)
        (* ... *)
    (* check stmts *)
    in let symbols = Map.fold_left (fun retval -> check_stmt snd retval) [] stmts 
    (* gather sstmt list *)
    in List.fold_left gather_stmt stmts

