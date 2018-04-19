open Ast

type sexpr = typ * sx (* sx is the same as `expr_detail` from lecture notes *)

and sx =
  | SId of string
  | SBinop of sexpr * op * sexpr
  | SUniop of uop * sexpr
  | SLit of int
  | SRealLit of float
  | SBoolLit of bool
  | SCharLit of char
  | SStringLit of string
  | SInterStringLit of string list * sexpr list
  | STupleLit of sexpr list
  | SSetLit of sexpr list
  | SSetBuilder of sexpr option * sstmt * sexpr
  | SArrayLit of sexpr list
  | SArrayRange of sexpr * sexpr option * sexpr
  | SArrayGet of sexpr * sexpr
  | SFuncDef of sstmt list * sstmt list (* formals to decl * function body *)
  | SFuncCall of string * sexpr

and sstmt =
  | SAsn of string * sexpr
  | SDecl of string * typ
  | SExpr of sexpr
  | SIter of sstmt list * sexpr (* decl list * set *)
  | SIf of sexpr * sstmt list * sstmt list
  | SWhile of sexpr * sstmt list

type sprogram = sstmt list
