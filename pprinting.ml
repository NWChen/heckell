open Ast

(* Pretty-printing function *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="

let string_of_prim_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Real -> "real"
  | Char -> "char"

let rec string_of_typ = function
    Set(t) -> string_of_typ t
  | Func(t1, t2) -> string_of_typ t1 ^ "->" ^ string_of_typ t2 ^ ";"
  | PrimTyp(t) -> string_of_prim_typ t

let rec string_of_expr = function
    Lit(l) -> string_of_int l
  | RealLit(l) -> l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | SetLit(el) -> 
     let rec string_of_expr_list = function
      | [e] -> string_of_expr e
      | e :: t -> string_of_expr e ^ ", " ^ string_of_expr_list t
      | [] -> ""
    in "{" ^ string_of_expr_list el ^ "}"
  | FuncDef(formals, stmts) ->
      (String.concat "\n" (List.map string_of_expr formals)) ^ "\n" ^ (String.concat "\n" (List.map string_of_stmt stmts))

and string_of_stmt = function
    Asn(s, e) -> s ^ " = " ^ string_of_expr e
  | Decl(s, t) -> "let " ^ s ^ ": " ^ string_of_typ t
  | Expr(e) -> string_of_expr e

let string_of_program stmts =
  (* let pretty_print_stmt = *)
  String.concat "\n" (List.map string_of_stmt stmts) ^ "\n"
  
