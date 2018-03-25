{ open Parser }

let letter = ['a'-'z' 'A'-'Z' '_' '\'']
let digit = ['0'-'9']

(* TODO: add `(...)` *)

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
| "and"     { AND }
| "or"      { OR }
| '|'       { PIPE } (* Types *)
| "int"     { INT }
| "real"    { REAL }
| "bool"    { BOOL }
| "char"    { CHAR }
| "set"     { SET }
| "array"   { ARRAY } 
| "let"     { LET }
| "in"      { IN }
| "..."     { ELLIPSE } 
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
