open Ast


type sexpr = typ * sx = (* sx is the same as `expr_detail` from lecture notes *)


and sx =
    SId of string
  | SBinop of expr * op * expr
  | SUniop of uop * expr
  | SLit of int
  | SRealLit of string
  | SBoolLit of bool
  | STupleLit of expr list
  | SSetLit of expr list
  | SSetBuilder of stmt * expr
  | SSetBuilderExt of expr * stmt * expr list
  | SFuncDef of expr list * stmt list (* param ids * function body *)
  | SFuncCall of string * expr list

and sstmt =
    SAsn of string * expr
  | SDecl of string * typ
  | SExpr of expr
  | SIter of string * expr

type sprogram = sstmt list
