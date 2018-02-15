type token =
  | LET
  | COLON
  | SEMI
  | INT
  | SET
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | ASN
  | EOF
  | LITERAL of (int)
  | VARIABLE of (string)

val expr :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.expr
