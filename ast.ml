type op = 
  Add | Sub | Mul | Div | Equal | Neq 
| Less | Leq | Greater | Geq | And | Or

type uop = Neg

type prim_typ = Int | Bool | Real | Char

type typ = 
  | Set of typ 
  | Tuple of typ list 
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
  | SetBuilder of stmt * expr
  | SetBuilderExt of expr * stmt * expr list
  | FuncDef of expr list * stmt list (* param ids * function body *)
  | FuncCall of string * expr list

and stmt =
    Asn of string * expr
  | Decl of string * typ
  | Expr of expr
  | Iter of string * expr

type program = stmt list
