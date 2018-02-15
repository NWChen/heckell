type op = 
  Add | Sub | Mul | Div 

type typ = prim_typ | der_typ

type prim_typ = Int | Bool | Real | Char

type der_typ =
  | Set of typ
  (* | Tuple of typ list *) 
  (* | Unit of prim_typ *)
 

type bind = string * typ

type expr =
    Binop of expr * op * expr
  | Lit of int
  | Decl of string * typ
  | Asn of string * expr
  | Id of string
  (* TODO: | Func of ... *)
  (* | Seq of expr * expr  *)

 type stmt =
    Block of stmt list
  | Expr of expr
  (* | Return of expr *)
  (* | If of expr * stmt * stmt *)
  (* | For of expr * expr * expr * stmt *)
  (* | While of expr * stmt *)



type program = stmt list

(* TODO: op for `->` (TYPE) *)
(* TODO: op for `(...)` (PARAMS) *)
