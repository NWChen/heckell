%{ open Ast %}

/* TODO: add ->, :, (...), */

%token LPAREN RPAREN
%token LBRACE RBRACE
%token LBRACKET RBRACKET 
%token LET COLON COMMA SEMI DSEMI ARROW
%token INT SET
%token PLUS MINUS TIMES DIVIDE EQUAL EOF
%token <int> LITERAL
%token EOF
%token <string> ID



/* TODO: Precedence and associativity */
%left EQUAL
%left PLUS MINUS
%left TIMES DIVIDE


%start stmt
%type <Ast.expr> stmt

%%

program:
  stmt_list EOF { $1 }

/*decls:
  | decls stmt { $2::$1 }*/

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }


typ:
  INT		 { Int }
| typ SET    { Set($1) }
| typ ARROW typ { Func($1, $3) }
/* Tuple type */

stmt:
  stmt SEMI stmt        { Seq($1, $3) }
| ID EQUAL expr SEMI      { Asn($1, $3) }
| LET ID COLON typ SEMI { ($2, $4) }  /* binding of variables and functions */
| ID LPAREN formal_opt RPAREN EQUAL stmt_list DSEMI  /* function assign definition */
    { Asn($1, FuncDef({
        formals: $3;
        body: $6;
      })) }


formal_opt:
  /* nothing */ { [] }
| formal_list   { List.rev $1 }

formal_list:
  formal_list COMMA ID  { $3 :: $1 }
| ID                    { [$1] }

expr:
  expr PLUS   expr      { Binop($1, Add, $3) }
| expr MINUS  expr      { Binop($1, Sub, $3) }
| expr TIMES  expr      { Binop($1, Mul, $3) }
| expr DIVIDE expr      { Binop($1, Div, $3) }
| LITERAL               { Lit($1) }
