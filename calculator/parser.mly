%{ open Ast %}

/* TODO: add ->, :, (...), */

%token LET COLON SEMI
%token INT SET
%token PLUS MINUS TIMES DIVIDE ASN EOF
%token <int> LITERAL
%token <string> VARIABLE
%token EOF

%left ASN
%left PLUS MINUS
%left TIMES DIVIDE

%start expr
%type <Ast.expr> expr

%%

program:
  decls EOF { $1 }

decls:
  | decls expr { $2::$1 }

typ:
  INT		 { Int }
| typ SET    { Set($1) }

expr:
  expr PLUS   expr SEMI { Binop($1, Add, $3) }
| expr MINUS  expr SEMI { Binop($1, Sub, $3) }
| expr TIMES  expr SEMI { Binop($1, Mul, $3) }
| expr DIVIDE expr SEMI { Binop($1, Div, $3) }
| expr SEMI   expr SEMI { Seq($1, $3) }
| LET VARIABLE COLON typ SEMI { Decl($2, $4) } /* We treat `vdecl`s as expressions for now, but will probably have to isolate this later */
| LITERAL          { Lit($1) }
| VARIABLE         { Var($1) }
| VARIABLE ASN expr SEMI { Asn($1, $3) }
