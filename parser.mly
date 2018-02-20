%{ open Ast %}

/* TODO: add ->, :, (...), */

%token LBRACKET RBRACKET 
%token LPAREN RPAREN
%token LBRACE RBRACE

%token LET IN COLON COMMA SEMI DSEMI ARROW
%token FORALL EXISTS
%token EQ NEQ LT LEQ GT GEQ AND OR
%token INT BOOL REAL CHAR
%token SET 

%token PLUS MINUS TIMES DIVIDE EQUAL PIPE
%token <int> LITERAL
%token <string> REALLIT
%token <bool> BOOLLIT
%token <string> ID
%token EOF

/* TODO: Precedence and associativity */
/* %nonassoc COLON */
%right SEMI
/*%right DSEMI*/
/*%left LET*/
%left COMMA
%right EQUAL

%left ARROW
%left SET
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
typ: 
  INT            { PrimTyp(Int) }
| BOOL           { PrimTyp(Bool) }
| REAL           { PrimTyp(Real) }
| CHAR           { PrimTyp(Char) }
| typ ARROW typ  { Func($1, $3) }
| typ SET        { Set($1) }
/* Tuple type */


expr:
  ID                    { Id($1) }
| LITERAL               { Lit($1) }
| REALLIT               { RealLit($1) }
| BOOLLIT               { BoolLit($1) }
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
<<<<<<< HEAD
| FORALL ID IN expr PIPE expr
    { FuncCall("EVAL_ALL_TRUE", [$4; $6]) } /* TODO: construct the builtin function `EVAL_ALL_TRUE` */
| EXISTS ID IN expr PIPE expr
    { FuncCall("EVAL_ANY_TRUE", [$4; $6]) } /* TODO: construct the builtin function `EVAL_ANY_TRUE` */
=======
| expr AND    expr      { Binop($1, And, $3) }
| expr OR     expr      { Binop($1, Or, $3) }
>>>>>>> f140f6d1ddc7486a7891a2075d8f5acb36672bd7
| LBRACE expr_list RBRACE { SetLit(List.rev $2) }
/* TODO: Allow for set of tuples */
| LBRACE ID IN expr PIPE expr RBRACE   
    { SetBuilder(Iter($2, $4), FuncDef([$2], [Expr($6)])) }
| LBRACE expr PIPE ID IN expr set_build_ext_cond RBRACE
    { SetBuilderExt(
        FuncDef([$4], [Expr($2)]), 
        Iter($4, $6),
        List.rev $7
    )}


stmt:
| expr SEMI                { Expr($1) }
| ID EQUAL expr SEMI       { Asn($1, $3) }
| LET ID COLON typ SEMI    { Decl($2, $4) }  /* binding of variables and functions */
| ID LPAREN formal_list RPAREN EQUAL func_stmt_list DSEMI
                           { Asn($1, FuncDef(List.rev $3, List.rev $6)) }


stmt_list:
  /* nothing */  { [] }
| stmt_list stmt { $2 :: $1 }

/*
stmt_list_ne:
| stmt           { [$1] }
| stmt_list stmt { $2 :: $1 }
*/

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

formal_list:
| ID                    { [$1] }
| formal_list COMMA ID  { $3 :: $1 }
