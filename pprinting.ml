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
  | Member -> "in"

let string_of_uop = function
    Neg -> "-"

let string_of_prim_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Real -> "real"
  | Char -> "char"

let rec string_of_typ = function
    Set(t) -> "(" ^ string_of_typ t ^ " set)"
  | Map(t1, t2) -> "(" ^ string_of_typ t1 ^ " -> " ^ string_of_typ t2 ^ " map)"
  | Func(t1, t2) -> "(" ^ string_of_typ t1 ^ " -> " ^ string_of_typ t2 ^ ")"
  | Tuple(tl) -> "(" ^ (String.concat " * " (List.map string_of_typ tl)) ^ ")" 
  | Array(t) -> "(" ^ string_of_typ t ^ " array)"
  | String -> "string"
  | PrimTyp(t) -> string_of_prim_typ t

let rec string_of_expr = function
    Lit(l) -> string_of_int l
  | RealLit(l) -> string_of_float l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | CharLit(c) -> "'" ^ Char.escaped c ^ "'"
  | StringLit(s) -> "\"" ^ s ^ "\""
  | InterStringLit(sl, el) -> 
      let rec interweave_print l1 l2 =
        match l1, l2 with
        | [s], _ -> s
        | h1::t1, h2::t2 -> 
          h1 ^ "\\( " ^ (string_of_expr h2) ^ " )" ^ interweave_print t1 t2
        | _ -> raise (Failure "heckin interpolated string")
      in "\"" ^ interweave_print sl el ^ "\""
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      "(" ^ string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2 ^ ")"
  | Uniop(o, e) -> string_of_uop o ^ string_of_expr e
  | FuncCall(s, e) -> (
    match e with
    | TupleLit(_) -> s ^ " " ^ string_of_expr e
    | x -> s ^ " (" ^ string_of_expr x ^ ")" )
  | SetLit(el) -> "{" ^ (String.concat ", " (List.map string_of_expr el)) ^ "}"
  | ArrayLit(el) -> "[" ^ (String.concat ", " (List.map string_of_expr el)) ^ "]"
  | ArrayRange(e1, e2, e3) ->
    (match e2 with 
      | None -> "[" ^ string_of_expr e1 ^ " ... " ^ string_of_expr e3 ^ "]"
      | Some x -> "[" ^ string_of_expr e1 
                  ^ ", " ^ string_of_expr x ^ " ... " 
                  ^ string_of_expr e3 ^ "]"
    )
  | TupleLit(el) -> "(" ^ (String.concat ", " (List.map string_of_expr el)) ^ ")"
  | SetBuilder(opt, s, e2) -> 
    (match opt with
      | None -> "{" ^ string_of_stmt s ^ " | " ^ string_of_expr e2 ^ "}"
      | Some e1 -> "{" ^ string_of_expr e1 
                  ^ " | " ^ string_of_stmt s 
                  ^ ", " ^ string_of_expr e2 ^ "}"
    )
  | AggAccessor(e1, e2) -> string_of_expr e1 ^ "[" ^ string_of_expr e2 ^ "]"
  | FuncDefNamed(_, formals, stmts) ->
      "(" ^ (String.concat "," formals) ^ ") ->\n  (\n    "
      ^ (String.concat ";\n    " (List.map string_of_stmt stmts)) ^ "\n  )"
  | _ -> "_"

and string_of_stmt = function
    Asn(sl, e) -> (String.concat "," sl) ^ " = " ^ string_of_expr e
  | Decl(s, t) -> "let " ^ s ^ ": " ^ string_of_typ t
  | AsnDecl(sl, e) -> "let " ^ (String.concat "," sl) ^ " = " ^ string_of_expr e
  | Expr(e) -> string_of_expr e
  | Iter(sl, e) -> (String.concat "," sl) ^ " in " ^ string_of_expr e
  | If(e, stmts, stmts2) -> "if " ^ (string_of_expr e) ^ " then\n " ^ (String.concat ";\n " (List.map string_of_stmt (List.rev stmts))) ^ "\n else\n " ^ (String.concat ";\n " (List.map string_of_stmt (List.rev stmts2)))
  | For(n, it, stmts) -> "for " ^ n ^ " in " ^ (string_of_expr it) ^ " then \n" ^ (String.concat ";\n " (List.map string_of_stmt (List.rev stmts)))

let string_of_program stmts =
  (* let pretty_print_stmt = *)
  String.concat "\n" (List.map string_of_stmt stmts) ^ "\n"


(* Sast pretty-printing *)

let rec string_of_sexpr (t, e) = 
  "(" ^ (match e with 
  | SLit(l) -> string_of_int l
  | SRealLit(l) -> string_of_float l
  | SBoolLit(true) -> "true"
  | SBoolLit(false) -> "false"
  | SCharLit(c) -> "'" ^ Char.escaped c ^ "'"
  | SStringLit(s) -> "\"" ^ s ^ "\""
  | SInterStringLit(sl, sel) -> 
      let rec interweave_print l1 l2 =
        match l1, l2 with
        | [s], _ -> s
        | h1::t1, h2::t2 -> 
          h1 ^ "\\( " ^ (string_of_sexpr h2) ^ " )" ^ interweave_print t1 t2
        | _ -> raise (Failure "heckin interpolated string")
      in "\"" ^ interweave_print sl sel ^ "\""
  | SId(s) -> s
  | SBinop(e1, o, e2) ->
      string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
  | SUniop(o, e) -> string_of_uop o ^ string_of_sexpr e
  | SFuncCall(s, e) | SMapCall(s, e) -> (
    match e with
    | (_, STupleLit(_)) -> s ^ " " ^ string_of_sexpr e
    | x -> s ^ " (" ^ string_of_sexpr x ^ ")" )
  | SSetLit(el) | SMapLit(el) -> "{" ^ (String.concat ", " (List.map string_of_sexpr el)) ^ "}"
  | SArrayLit(el) -> "[" ^ (String.concat ", " (List.map string_of_sexpr el)) ^ "]"
  | SArrayRange(e1, e2, e3) -> 
    (match e2 with 
      | None -> "[" ^ string_of_sexpr e1 ^ " ... " ^ string_of_sexpr e3 ^ "]"
      | Some x -> "[" ^ string_of_sexpr e1 ^ ", " 
                  ^ string_of_sexpr x ^ " ... " 
                  ^ string_of_sexpr e3 ^ "]")
  | STupleLit(el) -> "(" ^ (String.concat ", " (List.map string_of_sexpr el)) ^ ")"
  | SSetBuilder(opt, s, e2) -> 
    (match opt with
      | None -> "{" ^ string_of_sstmt s ^ " | " ^ string_of_sexpr e2 ^ "}"
      | Some e1 -> "{" ^ string_of_sexpr e1 
                  ^ " | " ^ string_of_sstmt s 
                  ^ ", " ^ string_of_sexpr e2 ^ "}"
    )
  | SAggAccessor(e1, e2) -> string_of_sexpr e1 ^ "[" ^ string_of_sexpr e2 ^ "]"
  | SFuncDef(formals, stmts) ->
      "(\n    " ^ (String.concat ";\n    " (List.map string_of_sstmt (formals@stmts))) ^ "\n  )"
  ) ^ " : " ^ (string_of_typ t) ^ ")"
and string_of_sstmt = function
  | SAsn(s, e) -> s ^ " = " ^ string_of_sexpr e
  | SDecl(s, t) -> "let " ^ s ^ ": " ^ string_of_typ t
  | SExpr(e) -> string_of_sexpr e
  | SIter(sl, e) -> 
    let helper = function
      | SDecl(s, t) -> s ^ ": " ^ string_of_typ t
      | _ -> raise(Failure("SIter should have SDecl only"))
    in let str_sdecl = List.map helper sl
    in "(" ^ (String.concat ", " str_sdecl) ^ ") in " ^ string_of_sexpr e
  | SIf(e, stmts, stmts2) -> "if " ^ (string_of_sexpr e) ^ " then\n " ^ (String.concat ";\n " (List.map string_of_sstmt stmts)) ^ "\n else\n " ^ (String.concat ";\n " (List.map string_of_sstmt stmts2))
  | _ -> "_"

let string_of_sprogram sstmts = 
  String.concat "\n" (List.map string_of_sstmt sstmts) ^ "\n"
  
