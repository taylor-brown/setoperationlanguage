%{ open Ast

 %}

%token LPAREN RPAREN COMMA LBRACE RBRACE
%token ASSIGN PLUS MINUS TIMES DIV MOD
%token EOF EOL AND NOT OR LTHAN GTHAN EQUALITY NSUB NEQUAL
%token IF ELSE FUNCTION END COLON 
%token <int> LITERAL
%token <string> STR
%token <string> ID
%token EOF

%left ASSIGN LPAREN RPAREN
%left EQUALITY NEQUAL
%left NOT
%left AND 
%left OR
%left LTHAN GTHAN
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
	FUNCTION ID formal_opt COLON stmt_list END
		{ { fname = $2;
				formals = $3;
				body = Block(List.rev $5)}} 
formal_opt:
	{[]}
	|formal_list {$1}

formal_list:
	/*|    {[] }*/
	|	ID {[$1]}
	| formal_list ID {$2 :: $1 }

/*vdecl:
	ID { $1 }*/

stmt_list:
	| stmt { [$1] }
	| stmt_list stmt  { $2 :: $1 }

stmt:
		expr {Expr($1)}
	| IF  expr COLON  stmt_list ELSE  stmt_list END { If($2, Block(List.rev $4), Block(List.rev $6))}


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
	| NOT expr { Binop($2, Not, Noexpr)}
	| expr OR expr { Binop($1, Or, $3)}
  | ID ASSIGN expr { Assign($1, $3)}
	| LITERAL { Literal($1) }
	| STR { Str($1) }
	| LBRACE optionals_list RBRACE { Set($2) }
	| ID LPAREN optionals_list RPAREN {Call($1, List.rev $3)}


optionals_list:
	 {[]}
	| actuals_list {$1}

actuals_list:
	  expr { [$1] }
	| actuals_list expr { $2 :: $1 }
