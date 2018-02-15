type op = 
  Add | Sub | Mul | Div

type typ = prim_typ | der_typ

type prim_typ = Int | Bool | Real | Char

type der_typ =
  | Set of typ
  (* | Tuple of typ list  *)
  (* | Unit of prim_typ *)
  | Func of typ * typ (* typ1: args, typ2: output *)
 

type bind = string * typ

type expr =
    Binop of expr * op * expr
  | Lit of int
  | Decl of string * typ
  | Asn of string * expr
  | Id of string
  | FuncDef of func_def
  (* | Seq of expr * expr  *)

type stmt =
    Seq of stmt * stmt
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

type func_def = {
  formals : expr list; (* id list *)
  body : stmt list; (* stmt list whose last stmt is expr that returns output typ *)
}

(* Program *)

type program = stmt list

(* TODO: op for `->` (TYPE) *)
(* TODO: op for `(...)` (PARAMS) *)
