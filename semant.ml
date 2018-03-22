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

    let type_of_identifier var map =
      try StringMap.find var map
      with Not_found -> raise (Failure ("undeclared identifier " ^ var))
    in
    (* Return a semantically-checked expression, i.e., with a type *)
    (* TODO: correct expr *)
    let rec expr e map = match e with
        Id s       -> (type_of_identifier s map, SId s)
        (* implement auto-boxing conditions *)
        | Binop (e1, op, e2) ->
            let (t1, e1') = expr e1 map
            and (t2, e2') = expr e2 map in
            (* All binary operators require operands of the same type *)
            let same = t1 = t2 in
            (* Determine expression type based on operator and operand types *)
            let ty = match op with
              Add | Sub | Mul | Div when same && t1 = PrimTyp(Int)  -> PrimTyp(Int)
            | Add | Sub | Mul | Div when same && t1 = PrimTyp(Real) -> PrimTyp(Real)
            | Equal | Neq            when same              -> PrimTyp(Bool)
            | Less | Leq | Greater | Geq
                       when same && (t1 = PrimTyp(Int) || t1 = PrimTyp(Real)) -> PrimTyp(Bool)
            | And | Or when same && t1 = PrimTyp(Bool) -> PrimTyp(Bool)
            | _ -> raise (Failure ("illegal binary operator")) (* TODO: full error statement *)
            in (ty, SBinop((t1, e1'), op, ((t2, e2')))) 
        | Uniop (op, e) ->
            let (t, e') = expr e map in
            let ty = match op with
              Neg when t = PrimTyp(Int) || t = PrimTyp(Real) || t = PrimTyp(Bool) -> t
            | _ -> raise (Failure ("illegal unary operator"))
            in (ty, SUniop(op, (t, e')))
        | Lit l -> (PrimTyp(Int), SLit l)
        | RealLit s -> (PrimTyp(Real), SRealLit s)
        | BoolLit b -> (PrimTyp(Bool), SBoolLit b)
    in
    let check_asn left_t right_t err =
        if left_t = right_t then left_t else raise (Failure err)
    in 
    let rec check_stmt to_check symbols = 
      match to_check with
      [] -> symbols
      | stmt :: tail -> match stmt with
        Decl (var, t) -> check_stmt tail (StringMap.add var t symbols)
        | Asn (var, e) ->
          let left_t = type_of_identifier var symbols
          and (right_t, e') = expr e symbols in
          let err = "illegal assignment " (* TODO rest of error message *)
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
      | Decl(t, var) -> SDecl(t, var)
    in 
    List.map append_sstmt stmts