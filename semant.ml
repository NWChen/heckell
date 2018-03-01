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
let check_stmt =
    let 

let check globals = 
    let check_binds (kind: string) (to_check: stmt list) =
    let check_it checked binding =
        let dup_err = "duplicate " ^ kind ^ " " ^ snd binding (* `snd binding` is the name of the duplicated variable *)
        in match binding with
            (_, n1) -> match checked with
                ((_, n2) :: _) when n1 = n2 -> raise (Failure dup_err)
                | _ -> binding :: checked (* add binding to list of checked bindings, named `checked` *)
    in let sorted = List.sort compare to_check in
        let _ = List.fold_left check_it [] sorted 
        in to_check
    in

    (* globals time *)
    let globals' = 
        check_binds "global" globals
    in

    (* check type of identifier *)
    let type_of_identifier s = 
        try StringMap.find s symbols
        with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* ensure types in an expr evaluation match *)
    let check_assign lvaluet rvaluet err =
        if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in

    (* ??? *)
    let rec expr = function
          Lit l -> (Int, SLit l)
        | Id s -> (type_of_identifier s, SId s)
    in

    let rec check_stmt = function
        Expr e -> SExpr (expr e)
        | Asn(var, e) as ex ->
            let lt = type_of_identifier var (* TODO type of identifier? *)
            and (rt, e') = expr e in
            let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^ string_of_typ rt ^ " in " ^ string_of_expr ex in
            check_assign lt rt err, SAssign(var, (rt, e'))
        | Decl(var, t) -> SDecl(var, t)
    in 

    (* TODO: the rest *)
    (* Populating list of bindings - transform `globals` into a StringMap *)
    let symbols = List.fold_left (fun m (t, name) -> StringMap.add name t m) StringMap.empty globals'
    in

    globals'
