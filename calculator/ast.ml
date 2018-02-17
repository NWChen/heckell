type operator = Add | Sub | Mul | Div

type prim_typ = Int | Bool | Real | Char

type typ = 
    Set of typ
  | Int
(*   | Unit of prim_typ
 *)

type bind = string * typ

type expr =
    Binop of expr * operator * expr
  | Lit of int
  | Id of string
  (* TODO: | Func of ... *)

type stmt =
    Seq of stmt * stmt
  | Asn of string * expr
  | Decl of string * typ
  | Expr of expr
  (* | Return of expr *)
  (* | If of expr * stmt * stmt *)
  (* | For of expr * expr * expr * stmt *)
  (* | While of expr * stmt *)


(* TODO: operator for `->` (TYPE) *)
(* TODO: operator for `(...)` (PARAMS) *)
