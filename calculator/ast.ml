type operator = Add | Sub | Mul | Div

type typ = Int (* and more, to be added ... *)
type bind = string * typ

type expr =
    Binop of expr * operator * expr
  | Lit of int
  | Decl of string * typ
  | Seq of expr * expr 
  | Asn of string * expr
  | Var of string

(* TODO: operator for `->` (TYPE) *)
(* TODO: operator for `(...)` (PARAMS) *)
