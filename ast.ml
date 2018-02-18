type op = 
  Add | Sub | Mul | Div | Equal | Neq | Less | Leq | Greater | Geq

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
    Id of string
  | Binop of expr * op * expr
  | Lit of int
  | RealLit of string
  | BoolLit of bool
  | SetLit of expr list
  | SetBuilder of stmt * expr
  | FuncDef of string list * stmt list (* param ids * function body *)
  | FuncCall of string * expr list
  (* | Seq of expr * expr  *)

and stmt =
    Asn of string * expr
  | Decl of string * typ
  | Expr of expr
  | Iter of string * expr

type program = stmt list
(* TODO: op for `->` (TYPE) *)
(* TODO: op for `(...)` (PARAMS) *)
