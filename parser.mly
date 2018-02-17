%{ open Ast %}

/* TODO: add ->, :, (...), */

%token LBRACKET RBRACKET 
%token LPAREN RPAREN
%token LBRACE RBRACE

%token LET COLON COMMA SEMI DSEMI ARROW
%token INT BOOL REAL CHAR SET 

%token PLUS MINUS TIMES DIVIDE EQUAL
%token <int> LITERAL
%token <string> ID
%token EOF

/* TODO: Precedence and associativity */
%nonassoc LET INT SET COLON
%right DSEMI
%right SEMI
%left COMMA
%right EQUAL
%right ARROW
%left PLUS MINUS
%left TIMES DIVIDE


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

expr:
  expr PLUS   expr      { Binop($1, Add, $3) }
| expr MINUS  expr      { Binop($1, Sub, $3) }
| expr TIMES  expr      { Binop($1, Mul, $3) }
| expr DIVIDE expr      { Binop($1, Div, $3) }
| LITERAL               { Lit($1) }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
  stmt SEMI stmt           { Seq($1, $3) }
| ID EQUAL expr SEMI       { Asn($1, $3) }
| LET ID COLON typ SEMI    { Decl($2, $4) }  /* binding of variables and functions */
| ID LPAREN formal_list RPAREN EQUAL stmt_list DSEMI  /* function assign definition */
                           { Asn($1, FuncDef($3, $6)) }

/*formal_opt:
                { [] }
| formal_list   { List.rev $1 }*/

formal_list:
  formal_list COMMA ID  { Id($3) :: $1 }
| ID                    { [Id($1)] }
