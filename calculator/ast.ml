type op = 
  Add | Sub | Mul | Div 

type typ = prim_typ | der_typ

type prim_typ = Int | Bool | Real | Char

type der_typ =
  | Set of typ
  | Tuple of typ list


(*   | Unit of prim_typ
 *)
type bind = string * typ

type expr =
    Binop of expr * op * expr
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


type program = expr list

(* TODO: op for `->` (TYPE) *)
(* TODO: op for `(...)` (PARAMS) *)
