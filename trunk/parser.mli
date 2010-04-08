type token =
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | COLON
  | COMMA
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | MOD
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
  | ASSIGN
  | EOF
  | EOL
  | LITERAL of (int)
  | ID of (string)
  | STR of (string)

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf ->  Ast.program
