{ open Parser }

let letter = ['a'-'z' 'A'-'Z' '_']
let digit = ['0'-'9']

(* TODO: add `:` *)
(* TODO: add `(...)` *)
(* TODOLATER: add `,` to delimit parameters *)

rule tokenize = parse
  [' ' '\t' '\r' '\n'] { tokenize lexbuf }
| '('       { LPAREN }
| ')'       { RPAREN }
| '{'       { LBRACE }
| '}'       { RBRACE }
| '['       { LBRACKET }
| ']'       { RBRACKET }
| '+'       { PLUS }
| '-'       { MINUS }
| '*'       { TIMES }
| '/'       { DIVIDE }
| "=="      { EQ }
| "!="      { NEQ }
| '<'       { LT }
| "<="      { LEQ }
| ">"       { GT }
| ">="      { GEQ }
| '|'       { PIPE } (* Types *)
| "int"     { INT }
| "real"    { REAL }
| "bool"    { BOOL }
| "set"     { SET }
| "let"     { LET }
| "in"      { IN }
| ','       { COMMA }
| ':'       { COLON }
| ';'       { SEMI }
| ";;"      { DSEMI }
| '='       { EQUAL }
| digit+ as lit   { LITERAL(int_of_string lit) }
| digit+ '.' digit+ as reallit { REALLIT(reallit) }
| "true"    { BOOLLIT(true) }
| "false"   { BOOLLIT(false) }
| letter (letter | digit)* as lit { ID(lit) }
| eof { EOF }
| "->"      { ARROW }
