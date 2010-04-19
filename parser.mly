%{ open Ast

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

program:
	/*| { [] }*/
	| fdecl { [$1] }
	| program fdecl {$2 :: $1}

fdecl:
	FUNCTION ID formal_list COLON stmt_list END
		{ { fname = $2;
				formals = $3;
				body = List.rev $5}} 
	
formal_list:
		ID {[$1]}
	| formal_list ID {$2 :: $1 }

/*vdecl:
	ID { $1 }*/

stmt_list:
	| stmt { [$1] }
	| stmt_list stmt  { $2 :: $1 }

stmt:
		expr {Expr($1)}
	| IF  expr COLON  stmt ELSE  stmt END { If($2, $4, $6)}


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
	| STR { Str($1) }
  /*| INT { Int($1) }
  | FLOAT { Float($1) } */
	| LBRACE optionals_list RBRACE { Set($2) }
	| ID LPAREN actuals_list RPAREN {Call($1, $3)}

optionals_list:
	 {[]}
	| expr { [$1] }
	| optionals_list expr { $2 :: $1 }

actuals_list:
	  expr { [$1] }
	| actuals_list expr { $2 :: $1 }
