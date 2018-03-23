open Ast
open Sast

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
  | FuncCall(s, el) -> s ^ "(" ^ (String.concat ", " (List.map string_of_expr el)) ^ ")"
  | SetLit(el) -> "{" ^ (String.concat ", " (List.map string_of_expr el)) ^ "}"
  | ArrayLit(el) -> "[" ^ (String.concat ", " (List.map string_of_expr el)) ^ "]"
  | ArrayRange(e1, e2, e3) -> 
    (match e2 with 
      | None -> "[" ^ string_of_expr e1 ^ " ... " ^ string_of_expr e3 ^ "]"
      | Some x -> "[" ^ string_of_expr e1 
                  ^ ", " ^ string_of_expr x ^ " ... " 
                  ^ string_of_expr e3 ^ "]")
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


(* Sast pretty-printing *)

let rec string_of_sexpr (t, e) = 
  "(" ^ (match e with 
  | SLit(l) -> string_of_int l
  | SRealLit(l) -> l
  | SBoolLit(true) -> "true"
  | SBoolLit(false) -> "false"
  | SId(s) -> s
  | SBinop(e1, o, e2) ->
      string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
  | SUniop(o, e) -> string_of_uop o ^ string_of_sexpr e
  | SFuncCall(s, el) -> s ^ "(" ^ (String.concat ", " (List.map string_of_sexpr el)) ^ ")"
  | SSetLit(el) -> "{" ^ (String.concat ", " (List.map string_of_sexpr el)) ^ "}"
  | SArrayLit(el) -> "[" ^ (String.concat ", " (List.map string_of_sexpr el)) ^ "]"
  | SArrayRange(e1, e2, e3) -> 
    (match e2 with 
      | None -> "[" ^ string_of_sexpr e1 ^ " ... " ^ string_of_sexpr e3 ^ "]"
      | Some x -> "[" ^ string_of_sexpr e1 ^ ", " 
                  ^ string_of_sexpr x ^ " ... " 
                  ^ string_of_sexpr e3 ^ "]")
  | STupleLit(el) -> "(" ^ (String.concat ", " (List.map string_of_sexpr el)) ^ ")"
  | SSetBuilder(s, e) -> "{" ^ string_of_sstmt s ^ " | " ^ string_of_sexpr e ^ "}"
  | SSetBuilderExt(e1, s, el) -> 
      let stmt_str = string_of_sstmt s in
      let expr_str_list = List.map string_of_sexpr el in
      let cond_str = String.concat ", " (stmt_str :: expr_str_list) in
      "{" ^ string_of_sexpr e1 ^ " | " ^ cond_str ^ "}"
  | SFuncDef(formals, stmts) ->
      "(" ^ (String.concat "," (List.map string_of_sexpr formals)) ^ ") ->\n  (\n    "
      ^ (String.concat ";\n    " (List.map string_of_sstmt stmts)) ^ "\n  )"
  ) ^ " : " ^ (string_of_typ t) ^ ")"
and string_of_sstmt = function
  | SAsn(s, e) -> s ^ " = " ^ string_of_sexpr e
  | SDecl(s, t) -> "let " ^ s ^ ": " ^ string_of_typ t
  | SExpr(e) -> string_of_sexpr e
  | SIter(s, e) -> s ^ " in " ^ string_of_sexpr e

let string_of_sprogram sstmts = 
  String.concat "\n" (List.map string_of_sstmt sstmts) ^ "\n"
  
