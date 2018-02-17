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
  | Id of string
  | FuncDef of func_def
  (* | Seq of expr * expr  *)

and func_def = {
  formals : expr list; (* id list *)
  body : stmt list; (* stmt list whose last stmt is expr that returns output typ *)
}

and stmt =
    Seq of stmt * stmt
  | Asn of string * expr
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
