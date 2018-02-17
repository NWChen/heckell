{ open Parser }

(* TODO: add `->` *)
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

| '=' { ASN }
| ['0'-'9']+ as lit { LITERAL(int_of_string lit) }
| ['a'-'z']+ as lit { ID(lit) }
| eof { EOF }
