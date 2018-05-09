%{ open Ast %}

/* TODO: add ->, :, (...), */

%token LBRACKET RBRACKET 
%token LPAREN RPAREN
%token LBRACE RBRACE

%token LET IN COLON COMMA SEMI DSEMI END ARROW	
%token EQ NEQ LT LEQ GT GEQ AND OR
%token INT BOOL REAL CHAR STRING
%token SET MAP
%token ARRAY

%token PLUS MINUS TIMES DIVIDE EQUAL PIPE ELLIPSE
%token IF THEN ELSE WHILE DO
%token <int> LITERAL
%token <float> REALLIT
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
%nonassoc ELSE
/* %nonassoc COLON */
%right SEMI
%right DSEMI END
/*%left LET*/
%left COMMA
%right EQUAL

%right MAP
%left ARROW
%left SET
%left BEGINTERSTRING
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left IN
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
| typ MAP             { match $1 with 
                        | Func(t1, t2) -> Map(t1, t2)
                        | _ -> raise(Failure "map qualifier can only be used on function type")
                      }


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
| expr IN     expr      { Binop($1, Member, $3) }
| expr LBRACKET expr RBRACKET
                        { AggAccessor($1, $3) }
| ID single_or_tuple    { FuncCall($1, $2) }
| single_or_tuple       { $1 }
| LBRACE expr_list RBRACE { SetLit(List.rev $2) }
| LBRACKET expr_list RBRACKET { ArrayLit(List.rev $2) }
| LBRACKET expr_list ELLIPSE expr RBRACKET 
    { match List.rev $2 with
        [e1] -> ArrayRange(e1, None, $4)
      | [e1; e2] -> ArrayRange(e1, Some e2, $4)
      | _ -> raise (Failure("Incompatible arguments for ArrayRange"))
    }
/* TODO: Allow for set of tuples */
| LBRACE expr PIPE expr_list_ne RBRACE   
    {
      let rec to_ids = function
        | h::t -> (
          match h with
          | Id(s) -> s::(to_ids t)
          | _ -> raise(Failure("an iterator must take an id")) )
        | [] -> []
      in match $4 with 
        | [] -> raise(Failure("at least one condition needed")) 
        | _ -> let builder_conds = List.rev $4
      in match $2 with
        | Binop(x, Member, set) -> (* iter | expr *)
          let elem = match x with
            | Id(s) -> [s]
            | TupleLit(l) -> to_ids l
            | _ -> raise(Failure("an iterator only takes ids"))
          in SetBuilder(
            None, Iter(elem, set),
            List.fold_left (fun e1 e2 -> Binop(e1, And, e2)) (List.hd builder_conds) (List.tl builder_conds)
            )
        | ex -> ( (* expr | iter *)
          match (List.hd builder_conds) with
          | Binop(x, Member, set) -> (
            let elem = match x with
              | Id(s) -> [s]
              | TupleLit(l) -> to_ids l
              | _ -> raise(Failure("an iterator only takes ids"))
            in let cond = match (List.tl builder_conds) with
              | [] -> BoolLit(true)
              | h::t -> List.fold_left (fun e1 e2 -> Binop(e1, And, e2)) (h) (t)
            in SetBuilder(Some ex, Iter(elem, set), cond) )
          | _ -> raise(Failure("iterator expected")) )
    }
  

single_or_tuple:
| LPAREN expr_list_ne RPAREN  { match $2 with 
                                | [x] -> x
                                | l -> TupleLit(List.rev l)
                              }


stmt:
| expr SEMI                 { Expr($1) }
| ID EQUAL expr SEMI        { Asn([$1], $3) }
| LET ID COLON typ SEMI     { Decl($2, $4) }  /* binding of variables and functions */
| LET ID EQUAL expr SEMI    { AsnDecl([$2], $4) }
| LET single_or_tuple EQUAL expr SEMI 
                            { let get_id id = match id with
                                | Id(s) -> s
                                | _  -> raise(Failure("Can't assign to non-id"))
                              in let ids = match $2 with
                                | TupleLit(l) -> List.map get_id l
                                | e -> [get_id e]
                              in AsnDecl(ids, $4) 
                            }
| single_or_tuple EQUAL expr SEMI 
                            { let get_id id = match id with
                                | Id(s) -> s
                                | _  -> raise(Failure("Can't assign to non-id"))
                              in let ids = match $1 with
                                | TupleLit(l) -> List.map get_id l
                                | e -> [get_id e]
                              in Asn(ids, $3) 
                            }
| ID LPAREN expr_list_ne RPAREN EQUAL func_stmt_list END
                            { Asn([$1], FuncDefNamed($1, 
                              (let check_id e = 
                                match e with
                                | Id(s) -> s
                                | _ -> raise(Failure("wrong function formals"))
                              in let formals = List.map check_id $3
                              in List.rev formals),
                              List.rev $6)) 
                            }
| IF expr THEN stmt_list ELSE stmt_list END   { If($2, List.rev $4, List.rev $6) }
| WHILE expr DO stmt_list END { While($2, List.rev $4) }

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



/*formal_opt:
                { [] }
| formal_list   { List.rev $1 }*/
/*
formal_list:
| ID                    { [$1] }
| formal_list COMMA ID  { $3 :: $1 }
*/
