type op = 
  Add | Sub | Mul | Div

type prim_typ = Int | Bool | Real | Char

type typ = 
  | Set of typ 
  (* | Tuple of typ list  *)
  (* | Unit of prim_typ *)
  | Func of typ * typ (* typ1: args, typ2: output *)
  | PrimTyp of prim_typ

(* type typ = prim_typ | der_typ *)

(* type bind = string * typ *)

type expr =
    Binop of expr * op * expr
  | Lit of int
  | RealLit of string
  | BoolLit of bool
  | Id of string
  | FuncDef of func_def

and func_def = {
  formals : expr list; (* id list *)
  body : stmt list; (* stmt list whose last stmt is expr that returns output typ *)
}

and stmt =
    Asn of string * expr
  | Decl of string * typ
  | Expr of expr

  (* | Return of expr *)
  (* | If of expr * stmt * stmt *)
  (* | For of expr * expr * expr * stmt *)
  (* | While of expr * stmt *)

(* Function stuff *)

(* type func_decl = {
  fname : string;
  typ : typ; (* int * int - > int *)
}
 *)

(* Program *)

type program = stmt list

(* TODO: op for `->` (TYPE) *)
(* TODO: op for `(...)` (PARAMS) *)


(* Pretty-printing function *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"

let string_of_prim_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Real -> "real"
  | Char -> "char"

let string_of_typ = function
    PrimTyp(t) -> string_of_prim_typ t
  | _ -> "other type"

let rec string_of_expr = function
    Lit(l) -> string_of_int l
  | RealLit(l) -> l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2

let string_of_stmt = function
    Asn(s, e) -> s ^ "=" ^ string_of_expr e
  | Decl(s, t) -> "let " ^ s ^ ": " ^ string_of_typ t
  | Expr(e) -> string_of_expr e

let string_of_program stmts =
  String.concat "" (List.map string_of_stmt stmts) ^ "\n"