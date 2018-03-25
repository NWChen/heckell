type op = 
  Add | Sub | Mul | Div | Equal | Neq 
| Less | Leq | Greater | Geq | And | Or

type uop = Neg

type prim_typ = Int | Bool | Real | Char

(* Maybe we should fuse prim_typ into typ *)
type typ = 
  | Set of typ 
  | Tuple of typ list 
  | Array of typ
  | Func of typ * typ (* typ1: args, typ2: output *)
  | PrimTyp of prim_typ

type expr =
    Id of string
  | Binop of expr * op * expr
  | Uniop of uop * expr
  | Lit of int
  | RealLit of string
  | BoolLit of bool
  | TupleLit of expr list
  | SetLit of expr list
  | ArrayLit of expr list
  | ArrayRange of expr * expr option * expr
  (* Both expr could be optional *)
  | SetBuilder of expr option * stmt * expr
  | FuncDef of expr list * stmt list (* param ids * function body *)
  | FuncCall of string * expr list
  (* | Seq of expr * expr  *)

and stmt =
    Asn of string * expr
  | Decl of string * typ
  | Expr of expr
  | Iter of string * expr

type program = stmt list
(* TODO: op for `(...)` (PARAMS) *)
