{ open Parser }

let letter = ['a'-'z' 'A'-'Z' '_']
let digit = ['0'-'9']

(* TODO: add `(...)` *)

rule tokenize = parse
| [' ' '\t' '\r' '\n']  { tokenize lexbuf }
| "/*"                  { comment lexbuf }
| "//" [^'\n']* '\n'    { tokenize lexbuf }
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
| '|'       { PIPE }
| "int"     { INT }
| "real"    { REAL }
| "bool"    { BOOL }
| "char"    { CHAR }
| "set"     { SET }
| "let"     { LET }
| "in"      { IN }
| ','       { COMMA }
| ':'       { COLON }
| ';'       { SEMI }
| ";;"      { DSEMI }
| '='       { EQUAL }
| digit+ as lit                 { LITERAL(int_of_string lit) }
| digit+ '.' digit+ as reallit  { REALLIT(reallit) }
| "true"    { BOOLLIT(true) }
| "false"   { BOOLLIT(false) }
| "'" ([^ '\'' '\\'] as c) "'"  { CHARLIT(c) }
| "'\\n'"   { CHARLIT('\n') }
| "'\\t'"   { CHARLIT('\t') }
| "'\\''"   { CHARLIT('\'') }
| "'\\\"'"  { CHARLIT('"') }
| "\\\\"    { CHARLIT('\\') }
| "'\\" (digit+ as d) "'" {
  let value = int_of_string d in
  if value > 255 then
    raise (Failure "character escape must be 0-255")
  else
    CHARLIT(Char.chr value)
}
| letter (letter | digit)* as lit { ID(lit) }
| '"' { STRINGLIT(str "" lexbuf) }
| eof { EOF }
| "\x2D\x3E"  { ARROW } (* arrow op *)


and str old_str = parse
  [^ '\n' '"' '\\']+ as c { str (old_str ^ c) lexbuf }
| "\\n" { str (old_str ^ "\n") lexbuf }
| "\\t" { str (old_str ^ "\t") lexbuf }
| "\\\"" { str (old_str ^ "\"") lexbuf }
| "\\'" { str (old_str ^ "\'") lexbuf }
| "\\" (digit+ as d) {
  let value = int_of_string d in
  if value > 255 then
    raise (Failure "character escape must be 0-255")
  else
    str (old_str ^ String.make 1 (Char.chr value)) lexbuf
}
| "\\\\" { str (old_str ^ "\\" ) lexbuf }
| "\\\n" { str (old_str ^ "\n") lexbuf }
| '"' { old_str }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char ^
  " in string literal")) }


and comment = parse
  "*/" { tokenize lexbuf }
| _    { comment lexbuf }


