%{ open Ast %}

/* TODO: add ->, :, (...), */

%token LBRACKET RBRACKET 
%token LPAREN RPAREN
%token LBRACE RBRACE

%token LET IN COLON COMMA SEMI DSEMI ARROW	
%token EQ NEQ LT LEQ GT GEQ AND OR
%token INT BOOL REAL CHAR STRING
%token SET 
%token ARRAY

%token PLUS MINUS TIMES DIVIDE EQUAL PIPE ELLIPSE
%token <int> LITERAL
%token <string> REALLIT
%token <char> CHARLIT
%token <bool> BOOLLIT
%token <string> STRINGLIT
/* types of interpolated strings */
%token <string> BEGINTERSTRING 
%token <string> MIDINTERSTRING 
%token <string> ENDINTERSTRING 
%token <string> ID
%token EOF

/* TODO: Precedence and associativity */
/* %nonassoc COLON */
%right SEMI
%right DSEMI
/*%left LET*/
%left COMMA
%right EQUAL

%left ARROW
%left SET
%left BEGINTERSTRING
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right NEG
%left LPAREN LBRACKET


%start program
%type <Ast.program> program

%%

program:
  stmt_list EOF { List.rev $1 }

/* note this type is typ, NOT prim_typ */

/* 
  type structure is an extremly simplified version
  of ocaml's type parsing rules (from source code),
  with a few modifications to match our semantics
  (mostly the function type) 
 */
simple_typ:
  INT                 { PrimTyp(Int) }
| BOOL                { PrimTyp(Bool) }
| REAL                { PrimTyp(Real) }
| CHAR                { PrimTyp(Char) }
| STRING              { String }
| simple_typ SET      { Set($1) }
| simple_typ ARRAY    { Array($1) }
| LPAREN typ RPAREN   { $2 }


simple_typ_or_tuple:
| simple_typ                  { $1 }
| simple_typ TIMES typ_list   { Tuple($1 :: (List.rev $3)) }

typ: 
| simple_typ_or_tuple { $1 }
| typ ARROW typ       { Func($1, $3) }


typ_list:
| simple_typ           { [$1] }
| typ_list TIMES simple_typ   { $3 :: $1 }

expr:
  ID                    { Id($1) }
| LITERAL               { Lit($1) }
| REALLIT               { RealLit($1) }
| BOOLLIT               { BoolLit($1) }
| CHARLIT               { CharLit($1) }
| string_lit            { $1 }
| expr PLUS   expr      { Binop($1, Add, $3) }
| expr MINUS  expr      { Binop($1, Sub, $3) }
| expr TIMES  expr      { Binop($1, Mul, $3) }
| expr DIVIDE expr      { Binop($1, Div, $3) }
| MINUS expr %prec NEG  { Uniop(Neg, $2) }
| expr EQ     expr      { Binop($1, Equal, $3) }
| expr NEQ    expr      { Binop($1, Neq,   $3) }
| expr LT     expr      { Binop($1, Less,  $3) }
| expr LEQ    expr      { Binop($1, Leq,   $3) }
| expr GT     expr      { Binop($1, Greater, $3) }
| expr GEQ    expr      { Binop($1, Geq,   $3) }
| expr AND    expr      { Binop($1, And, $3) }
| expr OR     expr      { Binop($1, Or, $3) }
| ID LPAREN expr_list_ne RPAREN { FuncCall($1, $3) }
| LPAREN expr_list RPAREN { TupleLit(List.rev $2) }
| LBRACE expr_list RBRACE { SetLit(List.rev $2) }
| LBRACKET expr_list RBRACKET { ArrayLit(List.rev $2) }
| LBRACKET expr_list_ne ELLIPSE expr RBRACKET 
    { match List.rev $2 with
        [e1] -> ArrayRange(e1, None, $4)
      | [e1; e2] -> ArrayRange(e1, Some e2, $4)
      | _ -> raise (Failure("Too many arguments for ArrayRange"))
    }
/* TODO: Allow for set of tuples */
| LBRACE ID IN expr PIPE expr set_build_ext_cond RBRACE   
    { SetBuilder(
        (* identity function *)
        None, 
        Iter($2, $4), 
        FuncDef([Id($2)], [Expr(
          List.fold_left (fun e1 e2 -> Binop(e1, And, e2)) $6 (List.rev $7)
        )])
      )}
| LBRACE expr PIPE ID IN expr set_build_ext_cond RBRACE
    { SetBuilder(
        Some(FuncDef([Id($4)], [Expr($2)])), 
        Iter($4, $6),
        FuncDef([Id($4)], [Expr(
          match (List.rev $7) with
          | [] -> BoolLit(true)
          | h::t -> List.fold_left (fun e1 e2 -> Binop(e1, And, e2)) (h) (t)
        )])
      )}


stmt:
| expr SEMI                { Expr($1) }
| ID EQUAL expr SEMI       { Asn($1, $3) }
| LET ID COLON typ SEMI    { Decl($2, $4) }  /* binding of variables and functions */
| ID LPAREN expr_list_ne RPAREN EQUAL func_stmt_list DSEMI
                           { Asn($1, FuncDef(List.rev $3, List.rev $6)) }

stmt_list:
  /* nothing */  { [] }
| stmt_list stmt { $2 :: $1 }

/*
stmt_list_ne:
| stmt           { [$1] }
| stmt_list stmt { $2 :: $1 }
*/

string_lit:
| STRINGLIT            { StringLit($1) }
| BEGINTERSTRING mid_inter_string  ENDINTERSTRING      
    { InterStringLit(
        $1::( List.rev ($3::(fst $2)) ), 
        List.rev (snd $2) ) 
    }

mid_inter_string:
| expr    { ([], [$1]) }
| mid_inter_string MIDINTERSTRING expr   
    { ($2::(fst $1), $3::(snd $1)) }


expr_list:
  /* nothing */        { [] }
| expr                 { [$1] }
| expr_list COMMA expr { $3 :: $1 }

expr_list_ne:
| expr                 { [$1] }
| expr_list COMMA expr { $3 :: $1 }

/* 
  This is tricky, all our stmts end with semicolon, however 
  the last stmt in this list should end without it as the
  next token in the func def is a double semi. expr on the
  other hand don't need to end in anything, so we can use
  them to specify the end of the list of stmts. Luckily this
  is the exact behavior we wanted for functions, as the last
  stmt has to be a expr which returns a value.
*/
func_stmt_list:
| stmt_list expr  { Expr($2) :: $1 }

set_build_ext_cond:
  /* nothing */       { [] }
| COMMA expr_list_ne  { $2 }


/*formal_opt:
                { [] }
| formal_list   { List.rev $1 }*/
/*
formal_list:
| ID                    { [$1] }
| formal_list COMMA ID  { $3 :: $1 }
*/
