%{ open Ast %}

/* TODO: add ->, :, (...), */

%token LET COLON SEMI
%token INT SET
%token PLUS MINUS TIMES DIVIDE ASN EOF
%token <int> LITERAL
%token EOF
%token <string> ID


%left ASN
%left PLUS MINUS
%left TIMES DIVIDE

%start stmt
%type <Ast.expr> stmt

%%

program:
  decls EOF { $1 }

decls:
  | decls stmt { $2::$1 }

typ:
  INT		 { Int }
| typ SET    { Set($1) }

stmt:
  stmt SEMI stmt        { Seq($1, $3) }
| ID ASN expr SEMI      { Asn($1, $3) }
| LET ID COLON typ SEMI { ($2, $4) }

expr:
  expr PLUS   expr      { Binop($1, Add, $3) }
| expr MINUS  expr      { Binop($1, Sub, $3) }
| expr TIMES  expr      { Binop($1, Mul, $3) }
| expr DIVIDE expr      { Binop($1, Div, $3) }
| LITERAL               { Lit($1) }
