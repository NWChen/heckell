{ open Parser }

let letter = ['a'-'z' 'A'-'Z' '_']
let digit = ['a'-'z' 'A'-'Z' '0'-'9']

(* TODO: add `:` *)
(* TODO: add `(...)` *)
(* TODOLATER: add `,` to delimit parameters *)

rule tokenize = parse
  [' ' '\t' '\r' '\n'] { tokenize lexbuf }
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIVIDE } (* Types *)
| "int" { INT }
| "set" { SET }
| "let" { LET }
| ':'   { COLON }
| ';'   { SEMI }
| '=' { EQUAL }
| digit+ as lit { LITERAL(int_of_string lit) }
| letter (letter | digit)* as lit { ID(lit) }
| eof { EOF }
| "->"  { ARROW }
