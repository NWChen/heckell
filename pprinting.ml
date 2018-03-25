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
  | And -> "and"
  | Or -> "or"

let string_of_uop = function
    Neg -> "-"

let string_of_prim_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Real -> "real"
  | Char -> "char"

let rec string_of_typ = function
    Set(t) -> "(" ^ string_of_typ t ^ " set)"
  | Func(t1, t2) -> "(" ^ string_of_typ t1 ^ " -> " ^ string_of_typ t2 ^ ")"
  | Tuple(tl) -> "(" ^ (String.concat " * " (List.map string_of_typ tl)) ^ ")" 
  | Array(t) -> "(" ^ string_of_typ t ^ " array)"
  | String -> "string"
  | PrimTyp(t) -> string_of_prim_typ t

let rec string_of_expr = function
    Lit(l) -> string_of_int l
  | RealLit(l) -> l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | CharLit(c) -> "'" ^ Char.escaped c ^ "'"
  | StringLit(s) -> "\"" ^ s ^ "\""
  | InterStringLit(sl, el) -> 
      let rec interleave_print l1 l2 =
        match l1, l2 with
        | [s], _ -> s
        | h1::t1, h2::t2 -> 
          h1 ^ "\\( " ^ (string_of_expr h2) ^ " )" ^ interleave_print t1 t2
        | _ -> raise (Failure "heckin interpolated string")
      in "\"" ^ interleave_print sl el ^ "\""
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Uniop(o, e) -> string_of_uop o ^ string_of_expr e
  | FuncCall(s, el) -> s ^ "(" ^ (String.concat ", " (List.map string_of_expr el)) ^ ")"
  | SetLit(el) -> "{" ^ (String.concat ", " (List.map string_of_expr el)) ^ "}"
  | ArrayLit(el) -> "[" ^ (String.concat ", " (List.map string_of_expr el)) ^ "]"
  | ArrayRange(e1, e2, e3) -> (match e2 with 
                              | None -> "[" ^ string_of_expr e1 ^ " ... " ^ string_of_expr e3 ^ "]"
                              | Some x -> "[" ^ string_of_expr e1 ^ ", " ^ string_of_expr x ^ " ... " ^ string_of_expr e3 ^ "]")
  | TupleLit(el) -> "(" ^ (String.concat ", " (List.map string_of_expr el)) ^ ")"
  | SetBuilder(s, e) -> "{" ^ string_of_stmt s ^ " | " ^ string_of_expr e ^ "}"
  | SetBuilderExt(e1, s, el) -> 
      let stmt_str = string_of_stmt s in
      let expr_str_list = List.map string_of_expr el in
      let cond_str = String.concat ", " (stmt_str :: expr_str_list) in
      "{" ^ string_of_expr e1 ^ " | " ^ cond_str ^ "}"
  | FuncDef(formals, stmts) ->
      "(" ^ (String.concat "," (List.map string_of_expr formals)) ^ ") ->\n  (\n    "
      ^ (String.concat ";\n    " (List.map string_of_stmt stmts)) ^ "\n  )"

and string_of_stmt = function
    Asn(s, e) -> s ^ " = " ^ string_of_expr e
  | Decl(s, t) -> "let " ^ s ^ ": " ^ string_of_typ t
  | Expr(e) -> string_of_expr e
  | Iter(s, e) -> s ^ " in " ^ string_of_expr e

let string_of_program stmts =
  (* let pretty_print_stmt = *)
  String.concat "\n" (List.map string_of_stmt stmts) ^ "\n"
  
