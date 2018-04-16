{ 
  open Parser
  type str_typ = Static | BegInter | MidInter | EndInter
  (* forgive me *)
  let paren_count = ref 0 (* allows for nested parens *)
  let inter_depth = ref 0 (* allows for nested strings *)
  (* 
    another solution would be creating a new rule
    of interpolated expressions but that would 
    require copying the tokenize rule and update
    the new rule whenever tokenize is updated

    Additional regex added to scanner can be
    done as usual
  *)
}

let letter = ['a'-'z' 'A'-'Z' '_' '\'']
let digit = ['0'-'9']
let exp = ['e''E'] ['+' '-']? digit+


rule tokenize = parse
| [' ' '\t' '\r' '\n']  { tokenize lexbuf }
| "/*"                  { comment lexbuf }
| "//" [^'\n']* '\n'    { tokenize lexbuf }
| '('       { 
  if !inter_depth > 0 then 
    paren_count := !paren_count + 1;
  LPAREN 
}
| ')'       { 
  if !inter_depth > 0 then
    if !paren_count = 0 then (
      inter_depth := !inter_depth - 1;
      let (styp, s) = str "" MidInter lexbuf in
      match styp with
      | MidInter -> MIDINTERSTRING(s)
      | EndInter -> ENDINTERSTRING(s)
      | _ -> raise (Failure "heckin string" )
    ) else ( paren_count := !paren_count - 1; RPAREN )
  else
    RPAREN 
}
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
| "array"   { ARRAY }
| "string"  { STRING }
| "let"     { LET }
| "in"      { IN }
| "..."     { ELLIPSE } 
| ','       { COMMA }
| ':'       { COLON }
| ';'       { SEMI }
| ";;"      { DSEMI }
| "end"     { END }
| '='       { EQUAL }
| "if"      { IF }
| "then"    { THEN }
| "else"    { ELSE }
| "while"   { WHILE }
| "do"      { DO }
| digit+ as lit                 { LITERAL(int_of_string lit) }
| (digit+ exp | (digit+ '.' digit* | '.' digit+) exp?) as reallit  
  { REALLIT(float_of_string reallit) }
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
| '"' { 
  let (styp, s) = str "" Static lexbuf in
    match styp with
    | Static -> STRINGLIT(s)
    | BegInter -> BEGINTERSTRING(s)
    | _ -> raise (Failure "heckin string" )
}
| eof { EOF }
| "\x2D\x3E"  { ARROW } (* arrow op *)


and str old_str typ = parse
  [^ '\n' '"' '\\']+ as c { str (old_str ^ c) typ lexbuf }
| "\\n" { str (old_str ^ "\n") typ lexbuf }
| "\\t" { str (old_str ^ "\t") typ lexbuf }
| "\\\"" { str (old_str ^ "\"") typ lexbuf }
| "\\'" { str (old_str ^ "\'") typ lexbuf }
| "\\(" {
  inter_depth := !inter_depth + 1;
  match typ with
  | Static -> (BegInter, old_str)
  | MidInter -> (MidInter, old_str)
  | _ -> raise (Failure "heckin string" )
}
| "\\" (digit+ as d) {
  let value = int_of_string d in
  if value > 255 then
    raise (Failure "character escape must be 0-255")
  else
    str (old_str ^ String.make 1 (Char.chr value)) typ lexbuf
}
| "\\\\" { str (old_str ^ "\\" ) typ lexbuf }
| "\\\n" { str (old_str ^ "\n") typ lexbuf }
| '"' { 
  match typ with
  | Static -> (Static, old_str)
  | MidInter -> (EndInter, old_str)
  | _ -> raise (Failure "heckin string" )
}
| _ as char { raise (Failure("illegal character " ^ Char.escaped char ^
  " in string literal")) }


and comment = parse
  "*/" { tokenize lexbuf }
| _    { comment lexbuf }
