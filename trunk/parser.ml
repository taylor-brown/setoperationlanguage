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

open Parsing;;
# 1 "parser.mly"
 open Ast

 
# 38 "parser.ml"
let yytransl_const = [|
  257 (* LPAREN *);
  258 (* RPAREN *);
  259 (* LBRACE *);
  260 (* RBRACE *);
  261 (* COLON *);
  262 (* COMMA *);
  263 (* PLUS *);
  264 (* MINUS *);
  265 (* TIMES *);
  266 (* DIV *);
  267 (* MOD *);
  268 (* AND *);
  269 (* NOT *);
  270 (* OR *);
  271 (* LTHAN *);
  272 (* GTHAN *);
  273 (* EQUALITY *);
  274 (* NSUB *);
  275 (* NEQUAL *);
  276 (* IF *);
  277 (* ELSE *);
  278 (* FUNCTION *);
  279 (* END *);
  280 (* ASSIGN *);
    0 (* EOF *);
  281 (* EOL *);
    0|]

let yytransl_block = [|
  282 (* LITERAL *);
  283 (* ID *);
  284 (* STR *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\003\000\003\000\003\000\004\000\004\000\
\005\000\005\000\006\000\006\000\006\000\006\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\006\000\007\000\007\000\
\007\000\008\000\008\000\000\000"

let yylen = "\002\000\
\001\000\002\000\006\000\000\000\001\000\002\000\001\000\002\000\
\001\000\007\000\001\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\001\000\001\000\003\000\004\000\000\000\001\000\
\002\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\001\000\000\000\002\000\005\000\
\000\000\000\000\006\000\000\000\000\000\000\000\027\000\000\000\
\028\000\000\000\007\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\003\000\008\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\026\000\029\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\014\000\015\000\016\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\030\000\000\000\
\000\000\000\000\010\000"

let yydgoto = "\002\000\
\004\000\005\000\009\000\018\000\019\000\020\000\023\000\047\000"

let yysindex = "\003\000\
\239\254\000\000\236\254\239\254\000\000\245\254\000\000\000\000\
\008\255\097\000\000\000\186\255\186\255\186\255\000\000\010\255\
\000\000\064\000\000\000\183\000\155\000\183\000\005\255\170\000\
\186\255\186\255\000\000\000\000\186\255\186\255\186\255\186\255\
\186\255\186\255\186\255\186\255\186\255\186\255\186\255\186\255\
\186\255\000\000\000\000\183\000\097\000\183\000\102\255\183\000\
\231\255\231\255\000\000\000\000\000\000\111\000\111\000\111\000\
\111\000\111\000\196\000\231\255\196\000\076\000\000\000\183\000\
\097\000\085\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\010\000\000\000\033\255\000\000\000\000\
\000\000\000\000\000\000\000\000\043\255\000\000\000\000\133\000\
\000\000\000\000\000\000\055\000\000\000\014\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\106\000\000\000\130\255\000\000\043\000\
\038\255\066\255\000\000\000\000\000\000\122\255\150\255\178\255\
\206\255\234\255\006\000\094\255\034\000\000\000\000\000\158\255\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\045\000\000\000\228\255\238\255\245\255\000\000\000\000"

let yytablesize = 470
let yytable = "\028\000\
\021\000\022\000\024\000\001\000\003\000\012\000\006\000\013\000\
\043\000\036\000\025\000\044\000\010\000\046\000\048\000\008\000\
\062\000\049\000\050\000\051\000\052\000\053\000\054\000\055\000\
\056\000\057\000\058\000\059\000\060\000\061\000\015\000\016\000\
\017\000\026\000\011\000\064\000\066\000\004\000\012\000\012\000\
\012\000\012\000\012\000\028\000\012\000\012\000\031\000\028\000\
\007\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\000\000\012\000\000\000\000\000\012\000\
\012\000\012\000\013\000\013\000\013\000\013\000\013\000\000\000\
\013\000\013\000\000\000\000\000\000\000\013\000\013\000\013\000\
\013\000\013\000\013\000\013\000\013\000\013\000\013\000\000\000\
\013\000\000\000\000\000\013\000\013\000\013\000\020\000\020\000\
\020\000\020\000\020\000\000\000\020\000\020\000\012\000\063\000\
\013\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
\020\000\020\000\020\000\000\000\020\000\000\000\000\000\020\000\
\020\000\020\000\022\000\022\000\022\000\022\000\022\000\015\000\
\016\000\017\000\034\000\034\000\034\000\022\000\022\000\022\000\
\022\000\022\000\022\000\000\000\022\000\022\000\022\000\000\000\
\022\000\000\000\000\000\022\000\022\000\022\000\023\000\023\000\
\023\000\023\000\023\000\034\000\034\000\034\000\035\000\035\000\
\035\000\023\000\023\000\023\000\023\000\023\000\023\000\000\000\
\023\000\023\000\023\000\000\000\023\000\000\000\000\000\023\000\
\023\000\023\000\024\000\024\000\024\000\024\000\024\000\035\000\
\035\000\035\000\012\000\000\000\013\000\024\000\024\000\024\000\
\024\000\024\000\024\000\000\000\024\000\024\000\024\000\000\000\
\024\000\000\000\000\000\024\000\024\000\024\000\019\000\019\000\
\019\000\019\000\019\000\015\000\016\000\017\000\000\000\000\000\
\000\000\019\000\019\000\019\000\019\000\019\000\019\000\000\000\
\019\000\019\000\019\000\000\000\019\000\000\000\000\000\019\000\
\019\000\019\000\018\000\018\000\018\000\018\000\018\000\031\000\
\032\000\033\000\000\000\000\000\000\000\018\000\018\000\018\000\
\018\000\018\000\018\000\000\000\018\000\018\000\018\000\000\000\
\018\000\000\000\000\000\018\000\018\000\018\000\017\000\017\000\
\017\000\017\000\017\000\000\000\000\000\000\000\032\000\000\000\
\032\000\032\000\000\000\000\000\000\000\000\000\017\000\000\000\
\017\000\017\000\017\000\000\000\017\000\000\000\000\000\017\000\
\017\000\017\000\021\000\021\000\021\000\021\000\021\000\032\000\
\032\000\032\000\000\000\025\000\025\000\025\000\025\000\025\000\
\000\000\000\000\021\000\000\000\021\000\021\000\021\000\009\000\
\021\000\009\000\000\000\021\000\021\000\021\000\025\000\025\000\
\012\000\025\000\013\000\000\000\025\000\025\000\025\000\000\000\
\000\000\000\000\009\000\009\000\012\000\009\000\013\000\000\000\
\009\000\009\000\009\000\014\000\000\000\012\000\027\000\013\000\
\000\000\015\000\016\000\017\000\000\000\000\000\000\000\014\000\
\065\000\012\000\000\000\013\000\000\000\015\000\016\000\017\000\
\014\000\000\000\033\000\067\000\033\000\033\000\015\000\016\000\
\017\000\000\000\000\000\000\000\014\000\029\000\030\000\031\000\
\032\000\033\000\015\000\016\000\017\000\000\000\000\000\000\000\
\040\000\000\000\000\000\033\000\033\000\033\000\011\000\011\000\
\011\000\011\000\000\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\000\000\011\000\042\000\000\000\011\000\011\000\
\011\000\029\000\030\000\031\000\032\000\033\000\034\000\035\000\
\036\000\037\000\038\000\039\000\040\000\041\000\045\000\000\000\
\029\000\030\000\031\000\032\000\033\000\034\000\035\000\036\000\
\037\000\038\000\039\000\040\000\041\000\029\000\030\000\031\000\
\032\000\033\000\034\000\035\000\036\000\037\000\038\000\039\000\
\040\000\041\000\029\000\030\000\031\000\032\000\033\000\034\000\
\035\000\036\000\037\000\038\000\000\000\040\000"

let yycheck = "\018\000\
\012\000\013\000\014\000\001\000\022\001\001\001\027\001\003\001\
\004\001\000\000\001\001\023\000\005\001\025\000\026\000\027\001\
\045\000\029\000\030\000\031\000\032\000\033\000\034\000\035\000\
\036\000\037\000\038\000\039\000\040\000\041\000\026\001\027\001\
\028\001\024\001\027\001\047\000\065\000\005\001\001\001\002\001\
\003\001\004\001\005\001\062\000\007\001\008\001\004\001\066\000\
\004\000\012\001\013\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\255\255\023\001\255\255\255\255\026\001\
\027\001\028\001\001\001\002\001\003\001\004\001\005\001\255\255\
\007\001\008\001\255\255\255\255\255\255\012\001\013\001\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\255\255\
\023\001\255\255\255\255\026\001\027\001\028\001\001\001\002\001\
\003\001\004\001\005\001\255\255\007\001\008\001\001\001\002\001\
\003\001\012\001\013\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\255\255\023\001\255\255\255\255\026\001\
\027\001\028\001\001\001\002\001\003\001\004\001\005\001\026\001\
\027\001\028\001\001\001\002\001\003\001\012\001\013\001\014\001\
\015\001\016\001\017\001\255\255\019\001\020\001\021\001\255\255\
\023\001\255\255\255\255\026\001\027\001\028\001\001\001\002\001\
\003\001\004\001\005\001\026\001\027\001\028\001\001\001\002\001\
\003\001\012\001\013\001\014\001\015\001\016\001\017\001\255\255\
\019\001\020\001\021\001\255\255\023\001\255\255\255\255\026\001\
\027\001\028\001\001\001\002\001\003\001\004\001\005\001\026\001\
\027\001\028\001\001\001\255\255\003\001\012\001\013\001\014\001\
\015\001\016\001\017\001\255\255\019\001\020\001\021\001\255\255\
\023\001\255\255\255\255\026\001\027\001\028\001\001\001\002\001\
\003\001\004\001\005\001\026\001\027\001\028\001\255\255\255\255\
\255\255\012\001\013\001\014\001\015\001\016\001\017\001\255\255\
\019\001\020\001\021\001\255\255\023\001\255\255\255\255\026\001\
\027\001\028\001\001\001\002\001\003\001\004\001\005\001\009\001\
\010\001\011\001\255\255\255\255\255\255\012\001\013\001\014\001\
\015\001\016\001\017\001\255\255\019\001\020\001\021\001\255\255\
\023\001\255\255\255\255\026\001\027\001\028\001\001\001\002\001\
\003\001\004\001\005\001\255\255\255\255\255\255\001\001\255\255\
\003\001\004\001\255\255\255\255\255\255\255\255\017\001\255\255\
\019\001\020\001\021\001\255\255\023\001\255\255\255\255\026\001\
\027\001\028\001\001\001\002\001\003\001\004\001\005\001\026\001\
\027\001\028\001\255\255\001\001\002\001\003\001\004\001\005\001\
\255\255\255\255\017\001\255\255\019\001\020\001\021\001\001\001\
\023\001\003\001\255\255\026\001\027\001\028\001\020\001\021\001\
\001\001\023\001\003\001\255\255\026\001\027\001\028\001\255\255\
\255\255\255\255\020\001\021\001\001\001\023\001\003\001\255\255\
\026\001\027\001\028\001\020\001\255\255\001\001\023\001\003\001\
\255\255\026\001\027\001\028\001\255\255\255\255\255\255\020\001\
\021\001\001\001\255\255\003\001\255\255\026\001\027\001\028\001\
\020\001\255\255\001\001\023\001\003\001\004\001\026\001\027\001\
\028\001\255\255\255\255\255\255\020\001\007\001\008\001\009\001\
\010\001\011\001\026\001\027\001\028\001\255\255\255\255\255\255\
\018\001\255\255\255\255\026\001\027\001\028\001\002\001\003\001\
\004\001\005\001\255\255\007\001\008\001\009\001\010\001\011\001\
\012\001\013\001\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\255\255\023\001\002\001\255\255\026\001\027\001\
\028\001\007\001\008\001\009\001\010\001\011\001\012\001\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\005\001\255\255\
\007\001\008\001\009\001\010\001\011\001\012\001\013\001\014\001\
\015\001\016\001\017\001\018\001\019\001\007\001\008\001\009\001\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\017\001\
\018\001\019\001\007\001\008\001\009\001\010\001\011\001\012\001\
\013\001\014\001\015\001\016\001\255\255\018\001"

let yynames_const = "\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  COLON\000\
  COMMA\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  MOD\000\
  AND\000\
  NOT\000\
  OR\000\
  LTHAN\000\
  GTHAN\000\
  EQUALITY\000\
  NSUB\000\
  NEQUAL\000\
  IF\000\
  ELSE\000\
  FUNCTION\000\
  END\000\
  ASSIGN\000\
  EOF\000\
  EOL\000\
  "

let yynames_block = "\
  LITERAL\000\
  ID\000\
  STR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 24 "parser.mly"
         ( [_1] )
# 292 "parser.ml"
               :  Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 :  Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 25 "parser.mly"
                 (_2 :: _1)
# 300 "parser.ml"
               :  Ast.program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'formal_list) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 29 "parser.mly"
  ( { fname = _2;
				formals = _3;
				body = List.rev _5})
# 311 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 34 "parser.mly"
      ([] )
# 317 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 35 "parser.mly"
      ([_1])
# 324 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'formal_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 36 "parser.mly"
                  (_2 :: _1 )
# 332 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 42 "parser.mly"
        ( [_1] )
# 339 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 43 "parser.mly"
                   ( _2 :: _1 )
# 347 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 46 "parser.mly"
       (Expr(_1))
# 354 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'stmt_list) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 47 "parser.mly"
                                                 ( If(_2, _4, _6))
# 363 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 51 "parser.mly"
      ( Id(_1) )
# 370 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 52 "parser.mly"
                  (Binop (_1, Plus, _3) )
# 378 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 53 "parser.mly"
                   (Binop (_1, Minus, _3) )
# 386 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 54 "parser.mly"
                   (Binop (_1, Times, _3) )
# 394 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 55 "parser.mly"
                 (Binop (_1, Divide, _3) )
# 402 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 56 "parser.mly"
                 (Binop (_1, Mod, _3) )
# 410 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 57 "parser.mly"
                      (Binop(_1, Equality, _3))
# 418 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 58 "parser.mly"
                   (Binop(_1, Gthan, _3))
# 426 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 59 "parser.mly"
                   (Binop(_1, Lthan, _3))
# 434 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 60 "parser.mly"
                  (Binop(_1, Nsub, _3))
# 442 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 61 "parser.mly"
                    (Binop(_1, Nequal, _3))
# 450 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 62 "parser.mly"
                 ( Binop(_1, And, _3))
# 458 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 63 "parser.mly"
                 ( Binop(_1, Not, _3))
# 466 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 64 "parser.mly"
                ( Binop(_1, Or, _3))
# 474 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 65 "parser.mly"
                   ( Assign(_1, _3))
# 482 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 66 "parser.mly"
                      (_2)
# 489 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 67 "parser.mly"
           ( Literal(_1) )
# 496 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 68 "parser.mly"
       ( Str(_1) )
# 503 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'optionals_list) in
    Obj.repr(
# 71 "parser.mly"
                                ( Set(_2) )
# 510 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_list) in
    Obj.repr(
# 72 "parser.mly"
                                 (Call(_1, _3))
# 518 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "parser.mly"
  ([])
# 524 "parser.ml"
               : 'optionals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 76 "parser.mly"
        ( [_1] )
# 531 "parser.ml"
               : 'optionals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'optionals_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 77 "parser.mly"
                       ( _2 :: _1 )
# 539 "parser.ml"
               : 'optionals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 80 "parser.mly"
        ( [_1] )
# 546 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 81 "parser.mly"
                     ( _2 :: _1 )
# 554 "parser.ml"
               : 'actuals_list))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf :  Ast.program)
