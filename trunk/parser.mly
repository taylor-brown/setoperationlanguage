%{ open Ast
let parse_error s= print_endline s
 %}

%token LPAREN RPAREN LBRACE RBRACE COLON COMMA PLUS MINUS TIMES DIV MOD
%token AND NOT OR LTHAN GTHAN EQUALITY NSUB NEQUAL IF ELSE FUNCTION END ASSIGN EOF EOL
%token <int> LITERAL
%token <string> ID
%token <string> STR

%left ASSIGN
%left EQUALITY NEQUAL
%left AND NOT OR LTHAN GTHAN
%left PLUS MINUS NSUB
%left TIMES DIV MOD

%start program
%type < Ast.program> program

%%
/*
program:
	EOL { Noexpr }
	| expr EOL { $1 }

expr:
	 LITERAL {Literal($1) }
	| LPAREN expr RPAREN { $2 }
	| ID LPAREN formal_list RPAREN { Call( $1, $3) }
	
formal_list:
	| LITERAL { Literal($1) }
*/


program:
	| fdecl { $1 }

fdecl:
	FUNCTION ID formal_list COLON stmt_list END
		{ { fname = $2;
				formals = $3;
				body = List.rev $5}} 
	
formal_list:
		ID {[$1]}
	| formal_list COMMA ID {$3 :: $1 }

vdecl:
	ID { $1 }

stmt_list:
		/*nothing { [] }*/
	| stmt_list stmt { $2 :: $1 }

stmt:
		expr EOL {Expr($1)}
	| IF expr COLON stmt ELSE stmt { If($2, $4, $6)}

expr:
	  ID { Id($1) }
	| expr PLUS expr {Binop ($1, Plus, $3) }
	| expr MINUS expr {Binop ($1, Minus, $3) }
	| expr TIMES expr {Binop ($1, Times, $3) }
	| expr DIV expr {Binop ($1, Divide, $3) }
	| expr MOD expr {Binop ($1, Mod, $3) }
	| expr EQUALITY expr {Binop($1, Equality, $3)}
	| expr GTHAN expr {Binop($1, Gthan, $3)}
	| expr LTHAN expr {Binop($1, Lthan, $3)}
	| expr NSUB expr {Binop($1, Nsub, $3)}
	| expr NEQUAL expr {Binop($1, Nequal, $3)}
	| expr AND expr { Binop($1, And, $3)}
	| expr NOT expr { Binop($1, Not, $3)}
	| expr OR expr { Binop($1, Or, $3)}
  | ID ASSIGN expr { Assign($1, $3)}
	| LPAREN expr RPAREN {$2}
	| LITERAL { Literal($1) }
  /*| INT { Int($1) }
  | FLOAT { Float($1) } */
	/*| LBRACE expr RBRACE { Set($2) }*/
	| ID LPAREN actuals_list RPAREN {Call($1, $3)}

/*expr_opt:
		/*nothing {Noexpr}
	/*| expr {$1}*/
	

actuals_list:
	  expr { [$1] }
	| actuals_list expr { $2 :: $1 }
