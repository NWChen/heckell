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
  | ID of (string)

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"
 open Ast 
# 21 "parser.ml"
let yytransl_const = [|
  257 (* LET *);
  258 (* COLON *);
  259 (* SEMI *);
  260 (* INT *);
  261 (* SET *);
  262 (* PLUS *);
  263 (* MINUS *);
  264 (* TIMES *);
  265 (* DIVIDE *);
  266 (* ASN *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  267 (* LITERAL *);
  268 (* ID *);
    0|]

let yylhs = "\255\255\
\002\000\002\000\001\000\001\000\001\000\003\000\003\000\003\000\
\003\000\003\000\000\000"

let yylen = "\002\000\
\001\000\002\000\003\000\004\000\005\000\003\000\003\000\003\000\
\003\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\010\000\000\000\000\000\001\000\000\000\004\000\000\000\
\000\000\000\000\000\000\005\000\002\000\000\000\000\000\008\000\
\009\000"

let yydgoto = "\002\000\
\005\000\014\000\011\000"

let yysindex = "\014\000\
\255\254\000\000\004\255\013\255\021\255\023\255\015\255\255\254\
\024\255\000\000\254\254\021\255\000\000\005\255\000\000\015\255\
\015\255\015\255\015\255\000\000\000\000\250\254\250\254\000\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\027\000\000\000\000\000\000\000\
\000\000\000\000\000\000\029\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\006\255\011\255\000\000\
\000\000"

let yygindex = "\000\000\
\022\000\000\000\003\000"

let yytablesize = 30
let yytable = "\003\000\
\015\000\018\000\019\000\016\000\017\000\018\000\019\000\020\000\
\006\000\021\000\004\000\006\000\006\000\007\000\001\000\006\000\
\007\000\007\000\022\000\023\000\024\000\025\000\007\000\008\000\
\009\000\010\000\011\000\013\000\003\000\012\000"

let yycheck = "\001\001\
\003\001\008\001\009\001\006\001\007\001\008\001\009\001\003\001\
\003\001\005\001\012\001\006\001\007\001\003\001\001\000\012\001\
\006\001\007\001\016\000\017\000\018\000\019\000\010\001\003\001\
\002\001\011\001\000\000\004\001\000\000\008\000"

let yynames_const = "\
  LET\000\
  COLON\000\
  SEMI\000\
  INT\000\
  SET\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  ASN\000\
  EOF\000\
  "

let yynames_block = "\
  LITERAL\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 21 "parser.mly"
        ( Int )
# 111 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    Obj.repr(
# 22 "parser.mly"
             ( Set(_1) )
# 118 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.stmt) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.stmt) in
    Obj.repr(
# 25 "parser.mly"
                        ( Seq(_1, _3) )
# 126 "parser.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 26 "parser.mly"
                        ( Asn(_1, _3) )
# 134 "parser.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    Obj.repr(
# 27 "parser.mly"
                        ( Decl(_2, _4) )
# 142 "parser.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 30 "parser.mly"
                        ( Binop(_1, Add, _3) )
# 150 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 31 "parser.mly"
                        ( Binop(_1, Sub, _3) )
# 158 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 32 "parser.mly"
                        ( Binop(_1, Mul, _3) )
# 166 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 33 "parser.mly"
                        ( Binop(_1, Div, _3) )
# 174 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 34 "parser.mly"
                        ( Lit(_1) )
# 181 "parser.ml"
               : 'expr))
(* Entry stmt *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let stmt (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.stmt)
