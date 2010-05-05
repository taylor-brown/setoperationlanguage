type token =
  | LPAREN
  | RPAREN
  | COMMA
  | LBRACE
  | RBRACE
  | ASSIGN
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | MOD
  | EOF
  | EOL
  | AND
  | NOT
  | OR
  | LTHAN
  | GTHAN
  | EQUALITY
  | NSUB
  | NEQUAL
  | IF
  | ELSE
  | FUNCTION
  | END
  | COLON
  | LITERAL of (int)
  | STR of (string)
  | ID of (string)

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf ->  Ast.program
