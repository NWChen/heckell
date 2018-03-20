open Ast

type sexpr = typ * sx (* sx is the same as `expr_detail` from lecture notes *)

and sx =
    SId of string
  | SBinop of sexpr * op * sexpr
  | SUniop of uop * sexpr
  | SLit of int
  | SRealLit of string
  | SBoolLit of bool
  | STupleLit of sexpr list
  | SSetLit of sexpr list
  | SSetBuilder of stmt * sexpr
  | SSetBuilderExt of sexpr * stmt * sexpr list
  | SFuncDef of sexpr list * stmt list (* param ids * function body *)
  | SFuncCall of string * sexpr list

and sstmt =
    SAsn of string * sexpr
  | SDecl of string * typ
  | SExpr of sexpr
  | SIter of string * sexpr

type sprogram = sstmt list
