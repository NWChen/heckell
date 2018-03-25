open Ast

type sexpr = typ * sx (* sx is the same as `expr_detail` from lecture notes *)

and sx =
    SId of string
  | SBinop of sexpr * op * sexpr
  | SUniop of uop * sexpr
  | SLit of int
  | SRealLit of string
  | SBoolLit of bool
  | SCharLit of char
  | SStringLit of string
  | SInterStringLit of string list * sexpr list
  | STupleLit of sexpr list
  | SSetLit of sexpr list
  | SSetBuilder of sexpr option * sstmt * sexpr
  | SArrayLit of sexpr list
  | SArrayRange of sexpr * sexpr option * sexpr
  | SFuncDef of sexpr list * sstmt list (* param ids * function body *)
  | SFuncCall of string * sexpr list

and sstmt =
    SAsn of string * sexpr
  | SDecl of string * typ
  | SExpr of sexpr
  | SIter of string * sexpr

type sprogram = sstmt list
