{ 
	open Parser
	open Lexing
	let incr_lineno lexbuf = 
		let pos = lexbuf.lex_curr_p in
		lexbuf.lex_curr_p <- {pos with
			 pos_lnum = pos.pos_lnum + 1;
			 pos_bol = pos.pos_cnum;
			}
	 }

rule token = parse
   [' ' '\t'] { token lexbuf }
	| ['\r' '\n']  {incr_lineno lexbuf; token lexbuf}
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
  | '/' { DIV }
  | '%' { MOD }
  | ',' { COMMA }
  | "=="{ EQUALITY }
  | "!="{ NEQUAL }  
  | '=' { ASSIGN }
  | ':' { COLON }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | "!<"{ NSUB }
  | '>' { GTHAN }
  | '<' { LTHAN }
  | '|' { OR }
  | '&' { AND }
  | '!' { NOT }
  | "function"  { FUNCTION }
  | "if" { IF }
  | "else" { ELSE }
  | "end" { END }
  | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
	| ['0' - '9']+ as lit { LITERAL(int_of_string lit) }
  (*| ['0' - '9']+ '.' ['0' - '9']+ as lit { FLOAT(float_of_string lit) }*)
  | '"' [^ '"']* '"' as lit { STR(String.sub lit 1 ((String.length lit)-2)) }
  | "//" { comment lexbuf } 
  | eof { EOF } 
  | _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
	 ['\r' '\n'] {token lexbuf}
	| _ { comment lexbuf }
