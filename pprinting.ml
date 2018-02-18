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

let string_of_uop = function
    Neg -> "-"

let string_of_prim_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Real -> "real"
  | Char -> "char"

let rec string_of_typ = function
    Set(t) -> "(" ^ string_of_typ t ^ " set)"
  | Func(t1, t2) -> "(" ^ string_of_typ t1 ^ ") -> (" ^ string_of_typ t2 ^ ")" ^ ";"
  | PrimTyp(t) -> string_of_prim_typ t

let rec string_of_expr = function
    Lit(l) -> string_of_int l
  | RealLit(l) -> l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Uniop(o, e) -> string_of_uop o ^ string_of_expr e
  | SetLit(el) -> "{" ^ (String.concat ", " (List.map string_of_expr el)) ^ "}"
  | SetBuilder(s, e) -> "{" ^ string_of_stmt s ^ " | " ^ string_of_expr e ^ "}"
  | FuncDef(formals, stmts) ->
      "(" ^ (String.concat "," formals) ^ ") ->\n  (\n    "
      ^ (String.concat ";\n    " (List.map string_of_stmt stmts)) ^ "\n  )"

and string_of_stmt = function
    Asn(s, e) -> s ^ " = " ^ string_of_expr e
  | Decl(s, t) -> "let " ^ s ^ ": " ^ string_of_typ t
  | Expr(e) -> string_of_expr e
  | Iter(s, e) -> s ^ " in " ^ string_of_expr e

let string_of_program stmts =
  (* let pretty_print_stmt = *)
  String.concat "\n" (List.map string_of_stmt stmts) ^ "\n"
  
