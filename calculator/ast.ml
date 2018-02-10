type operator = Add | Sub | Mul | Div

(* type prim_typ = Int | Bool | Real | Char
 *)
type typ = 
    Set of typ
  | Int
(*   | Unit of prim_typ
 *)
type bind = string * typ

type expr =
    Binop of expr * operator * expr
  | Lit of int
  | Decl of string * typ
  | Seq of expr * expr 
  | Asn of string * expr
  | Var of string

 type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

(* TODO: operator for `->` (TYPE) *)
(* TODO: operator for `(...)` (PARAMS) *)
