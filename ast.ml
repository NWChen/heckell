type op = 
  Add | Sub | Mul | Div | Equal | Neq 
| Less | Leq | Greater | Geq | And | Or

type uop = Neg

type prim_typ = Int | Bool | Real | Char

type typ = 
  | Set of typ 
  | Tuple of typ list 
  | Array of typ
  | String
  | Func of typ * typ (* typ1: args, typ2: output *)
  | PrimTyp of prim_typ

type expr =
    Id of string
  | Binop of expr * op * expr
  | Uniop of uop * expr
  | Lit of int
  | RealLit of string
  | BoolLit of bool
  | CharLit of char
  | StringLit of string
  | InterStringLit of string list * expr list
  | TupleLit of expr list
  | SetLit of expr list
<<<<<<< HEAD
  | ArrayLit of expr list
  | ArrayRange of expr * expr option * expr
  (* Both expr could be optional *)
  | SetBuilder of expr option * stmt * expr
  | FuncDef of expr list * stmt list (* param ids * function body *)
  | FuncCall of string * expr
  (* | Seq of expr * expr  *)
=======
  | SetBuilder of stmt * expr
  | SetBuilderExt of expr * stmt * expr list
  | FuncDef of expr list * stmt list (* param ids * function body *)
  | FuncCall of string * expr list
>>>>>>> 42bef3713c4d47c72ee6cfe466dd3da43a43c41b

and stmt =
    Asn of string * expr
  | Decl of string * typ
  | Expr of expr
  | Iter of string * expr

type program = stmt list
