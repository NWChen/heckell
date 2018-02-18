%{ open Ast %}

/* TODO: add ->, :, (...), */

%token LBRACKET RBRACKET 
%token LPAREN RPAREN
%token LBRACE RBRACE

%token LET IN COLON COMMA SEMI DSEMI ARROW
%token INT BOOL REAL CHAR
%token SET 

%token PLUS MINUS TIMES DIVIDE EQUAL PIPE
%token <int> LITERAL
%token <string> REALLIT
%token <bool> BOOLLIT
%token <string> ID
%token EOF

/* TODO: Precedence and associativity */
%nonassoc COLON
%right DSEMI
%right SEMI
%left LET
%left COMMA
%right EQUAL
%right ARROW
%left PLUS MINUS
%left TIMES DIVIDE
%left LPAREN LBRACKET


%start program
%type <Ast.program> program

%%

program:
  stmt_list EOF { $1 }

/* note this type is typ, NOT prim_typ */
typ: 
  INT            { PrimTyp(Int) }
| BOOL           { PrimTyp(Bool) }
| REAL           { PrimTyp(Real) }
| CHAR           { PrimTyp(Char) }
| typ ARROW typ  { Func($1, $3) }
| typ SET        { Set($1) }
/* Tuple type */


stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }


expr_list:
    /* nothing */        { [] }
  | expr                 { [$1] }
  | expr_list COMMA expr { $3 :: $1 }


expr:
| LBRACE expr_list RBRACE { SetLit($2) }
| ID                    { Id($1) }
| LITERAL               { Lit($1) }
| REALLIT               { RealLit($1) }
| BOOLLIT               { BoolLit($1) }
| expr PLUS   expr      { Binop($1, Add, $3) }
| expr MINUS  expr      { Binop($1, Sub, $3) }
| expr TIMES  expr      { Binop($1, Mul, $3) }
| expr DIVIDE expr      { Binop($1, Div, $3) }
/* TODO: Allow for set of tuples */
| LBRACE ID IN expr PIPE expr RBRACE   
    { SetBuilder(Iter($2, $4), FuncDef([$2], [Expr($6)])) }


stmt:
| ID EQUAL expr SEMI       { Asn($1, $3) }
| LET ID COLON typ SEMI    { Decl($2, $4) }  /* binding of variables and functions */
| ID LPAREN formal_list RPAREN EQUAL stmt_list DSEMI  /* function assign definition */
                           { Asn($1, FuncDef($3, $6)) }
| expr SEMI                { Expr($1) }

/*formal_opt:
                { [] }
| formal_list   { List.rev $1 }*/

formal_list:
  formal_list COMMA ID  { $3 :: $1 }
| ID                    { [$1] }
