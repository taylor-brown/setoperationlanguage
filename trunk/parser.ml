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
  | ID of (string)
  | STR of (string)

open Parsing;;
# 1 "parser.mly"
 open Ast

 
# 38 "parser.ml"
let yytransl_const = [|
  257 (* LPAREN *);
  258 (* RPAREN *);
  259 (* COMMA *);
  260 (* LBRACE *);
  261 (* RBRACE *);
  262 (* ASSIGN *);
  263 (* PLUS *);
  264 (* MINUS *);
  265 (* TIMES *);
  266 (* DIV *);
  267 (* MOD *);
    0 (* EOF *);
  268 (* EOL *);
  269 (* AND *);
  270 (* NOT *);
  271 (* OR *);
  272 (* LTHAN *);
  273 (* GTHAN *);
  274 (* EQUALITY *);
  275 (* NSUB *);
  276 (* NEQUAL *);
  277 (* IF *);
  278 (* ELSE *);
  279 (* FUNCTION *);
  280 (* END *);
  281 (* COLON *);
    0|]

let yytransl_block = [|
  282 (* LITERAL *);
  283 (* ID *);
  284 (* STR *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\003\000\003\000\004\000\004\000\005\000\
\005\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\006\000\006\000\006\000\006\000\007\000\007\000\008\000\008\000\
\000\000"

let yylen = "\002\000\
\001\000\002\000\006\000\001\000\002\000\001\000\002\000\001\000\
\007\000\001\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\002\000\003\000\003\000\
\001\000\001\000\003\000\004\000\000\000\001\000\001\000\002\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\001\000\000\000\002\000\004\000\
\000\000\000\000\005\000\000\000\000\000\000\000\025\000\000\000\
\026\000\000\000\006\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\003\000\007\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\027\000\000\000\000\000\000\000\000\000\000\000\000\000\
\013\000\014\000\015\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\028\000\000\000\000\000\009\000"

let yydgoto = "\002\000\
\004\000\005\000\009\000\018\000\019\000\020\000\022\000\023\000"

let yysindex = "\003\000\
\243\254\000\000\240\254\243\254\000\000\005\255\000\000\000\000\
\049\255\147\000\000\000\088\000\088\000\088\000\000\000\012\255\
\000\000\120\000\000\000\188\000\188\000\028\255\088\000\254\254\
\169\000\088\000\088\000\000\000\000\000\088\000\088\000\088\000\
\088\000\088\000\088\000\088\000\088\000\088\000\088\000\088\000\
\088\000\000\000\188\000\147\000\034\000\188\000\026\255\026\255\
\000\000\000\000\000\000\254\254\254\254\254\254\254\254\202\000\
\026\255\202\000\128\000\000\000\147\000\139\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\038\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\034\255\000\000\000\000\000\000\038\255\
\000\000\000\000\000\000\109\000\083\000\000\000\045\255\146\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\094\000\000\000\000\000\079\000\065\255\092\255\
\000\000\000\000\000\000\173\255\200\255\227\255\254\255\025\000\
\119\255\052\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\057\000\000\000\226\255\238\255\245\255\000\000\042\000"

let yytablesize = 477
let yytable = "\029\000\
\021\000\024\000\025\000\001\000\030\000\031\000\032\000\033\000\
\034\000\003\000\006\000\043\000\026\000\059\000\021\000\046\000\
\040\000\027\000\047\000\048\000\049\000\050\000\051\000\052\000\
\053\000\054\000\055\000\056\000\057\000\058\000\062\000\008\000\
\042\000\043\000\032\000\033\000\034\000\033\000\029\000\010\000\
\029\000\010\000\010\000\029\000\010\000\010\000\010\000\010\000\
\010\000\030\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\007\000\010\000\010\000\010\000\
\010\000\010\000\011\000\045\000\011\000\011\000\000\000\011\000\
\011\000\010\000\000\000\011\000\000\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\000\000\
\011\000\011\000\011\000\011\000\011\000\012\000\000\000\012\000\
\012\000\000\000\012\000\012\000\000\000\000\000\000\000\000\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\000\000\012\000\012\000\012\000\012\000\012\000\
\019\000\000\000\019\000\019\000\000\000\019\000\019\000\000\000\
\000\000\000\000\000\000\019\000\019\000\019\000\019\000\019\000\
\019\000\019\000\019\000\019\000\019\000\000\000\019\000\019\000\
\019\000\019\000\019\000\022\000\000\000\022\000\022\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\022\000\022\000\
\022\000\022\000\022\000\022\000\000\000\022\000\022\000\022\000\
\000\000\022\000\022\000\022\000\022\000\022\000\021\000\000\000\
\021\000\021\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\021\000\021\000\021\000\021\000\021\000\021\000\000\000\
\021\000\021\000\021\000\000\000\021\000\021\000\021\000\021\000\
\021\000\023\000\000\000\023\000\023\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\023\000\023\000\023\000\023\000\
\023\000\023\000\000\000\023\000\023\000\023\000\000\000\023\000\
\023\000\023\000\023\000\023\000\018\000\000\000\018\000\018\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\018\000\
\018\000\018\000\018\000\018\000\018\000\000\000\018\000\018\000\
\018\000\000\000\018\000\018\000\018\000\018\000\018\000\017\000\
\000\000\017\000\017\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\017\000\017\000\017\000\017\000\017\000\017\000\
\000\000\017\000\017\000\017\000\000\000\017\000\017\000\017\000\
\017\000\017\000\016\000\000\000\016\000\016\000\000\000\000\000\
\000\000\000\000\000\000\060\000\000\000\012\000\016\000\000\000\
\000\000\000\000\016\000\000\000\016\000\016\000\016\000\013\000\
\016\000\016\000\016\000\016\000\016\000\020\000\000\000\020\000\
\020\000\000\000\000\000\015\000\016\000\017\000\000\000\000\000\
\000\000\020\000\000\000\000\000\000\000\020\000\000\000\020\000\
\020\000\020\000\000\000\020\000\020\000\020\000\020\000\020\000\
\024\000\000\000\024\000\024\000\031\000\000\000\031\000\031\000\
\000\000\000\000\000\000\012\000\024\000\000\000\000\000\032\000\
\031\000\032\000\032\000\024\000\024\000\013\000\024\000\024\000\
\024\000\024\000\024\000\032\000\031\000\031\000\031\000\000\000\
\008\000\015\000\016\000\017\000\000\000\000\000\000\000\032\000\
\032\000\032\000\008\000\012\000\000\000\000\000\000\000\000\000\
\000\000\008\000\008\000\012\000\008\000\013\000\008\000\008\000\
\008\000\000\000\000\000\000\000\014\000\013\000\012\000\028\000\
\000\000\015\000\016\000\017\000\014\000\061\000\012\000\000\000\
\013\000\015\000\016\000\017\000\000\000\000\000\000\000\014\000\
\013\000\000\000\063\000\000\000\015\000\016\000\017\000\014\000\
\000\000\000\000\000\000\000\000\015\000\016\000\017\000\030\000\
\031\000\032\000\033\000\034\000\000\000\035\000\000\000\036\000\
\037\000\038\000\039\000\040\000\041\000\000\000\000\000\000\000\
\000\000\044\000\030\000\031\000\032\000\033\000\034\000\000\000\
\035\000\000\000\036\000\037\000\038\000\039\000\040\000\041\000\
\030\000\031\000\032\000\033\000\034\000\000\000\035\000\000\000\
\036\000\037\000\038\000\000\000\040\000"

let yycheck = "\018\000\
\012\000\013\000\014\000\001\000\007\001\008\001\009\001\010\001\
\011\001\023\001\027\001\023\000\001\001\044\000\026\000\027\000\
\019\001\006\001\030\000\031\000\032\000\033\000\034\000\035\000\
\036\000\037\000\038\000\039\000\040\000\041\000\061\000\027\001\
\005\001\045\000\009\001\010\001\011\001\000\000\005\001\002\001\
\059\000\004\001\005\001\062\000\007\001\008\001\009\001\010\001\
\011\001\005\001\013\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\004\000\024\001\025\001\026\001\
\027\001\028\001\002\001\026\000\004\001\005\001\255\255\007\001\
\008\001\025\001\255\255\027\001\255\255\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\255\255\
\024\001\025\001\026\001\027\001\028\001\002\001\255\255\004\001\
\005\001\255\255\007\001\008\001\255\255\255\255\255\255\255\255\
\013\001\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\255\255\024\001\025\001\026\001\027\001\028\001\
\002\001\255\255\004\001\005\001\255\255\007\001\008\001\255\255\
\255\255\255\255\255\255\013\001\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\255\255\024\001\025\001\
\026\001\027\001\028\001\002\001\255\255\004\001\005\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\013\001\014\001\
\015\001\016\001\017\001\018\001\255\255\020\001\021\001\022\001\
\255\255\024\001\025\001\026\001\027\001\028\001\002\001\255\255\
\004\001\005\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\013\001\014\001\015\001\016\001\017\001\018\001\255\255\
\020\001\021\001\022\001\255\255\024\001\025\001\026\001\027\001\
\028\001\002\001\255\255\004\001\005\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\013\001\014\001\015\001\016\001\
\017\001\018\001\255\255\020\001\021\001\022\001\255\255\024\001\
\025\001\026\001\027\001\028\001\002\001\255\255\004\001\005\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\013\001\
\014\001\015\001\016\001\017\001\018\001\255\255\020\001\021\001\
\022\001\255\255\024\001\025\001\026\001\027\001\028\001\002\001\
\255\255\004\001\005\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\013\001\014\001\015\001\016\001\017\001\018\001\
\255\255\020\001\021\001\022\001\255\255\024\001\025\001\026\001\
\027\001\028\001\002\001\255\255\004\001\005\001\255\255\255\255\
\255\255\255\255\255\255\002\001\255\255\004\001\014\001\255\255\
\255\255\255\255\018\001\255\255\020\001\021\001\022\001\014\001\
\024\001\025\001\026\001\027\001\028\001\002\001\255\255\004\001\
\005\001\255\255\255\255\026\001\027\001\028\001\255\255\255\255\
\255\255\014\001\255\255\255\255\255\255\018\001\255\255\020\001\
\021\001\022\001\255\255\024\001\025\001\026\001\027\001\028\001\
\002\001\255\255\004\001\005\001\002\001\255\255\004\001\005\001\
\255\255\255\255\255\255\004\001\014\001\255\255\255\255\002\001\
\014\001\004\001\005\001\021\001\022\001\014\001\024\001\025\001\
\026\001\027\001\028\001\014\001\026\001\027\001\028\001\255\255\
\004\001\026\001\027\001\028\001\255\255\255\255\255\255\026\001\
\027\001\028\001\014\001\004\001\255\255\255\255\255\255\255\255\
\255\255\021\001\022\001\004\001\024\001\014\001\026\001\027\001\
\028\001\255\255\255\255\255\255\021\001\014\001\004\001\024\001\
\255\255\026\001\027\001\028\001\021\001\022\001\004\001\255\255\
\014\001\026\001\027\001\028\001\255\255\255\255\255\255\021\001\
\014\001\255\255\024\001\255\255\026\001\027\001\028\001\021\001\
\255\255\255\255\255\255\255\255\026\001\027\001\028\001\007\001\
\008\001\009\001\010\001\011\001\255\255\013\001\255\255\015\001\
\016\001\017\001\018\001\019\001\020\001\255\255\255\255\255\255\
\255\255\025\001\007\001\008\001\009\001\010\001\011\001\255\255\
\013\001\255\255\015\001\016\001\017\001\018\001\019\001\020\001\
\007\001\008\001\009\001\010\001\011\001\255\255\013\001\255\255\
\015\001\016\001\017\001\255\255\019\001"

let yynames_const = "\
  LPAREN\000\
  RPAREN\000\
  COMMA\000\
  LBRACE\000\
  RBRACE\000\
  ASSIGN\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  MOD\000\
  EOF\000\
  EOL\000\
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
  COLON\000\
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
# 27 "parser.mly"
         ( [_1] )
# 291 "parser.ml"
               :  Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 :  Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 28 "parser.mly"
                 (_2 :: _1)
# 299 "parser.ml"
               :  Ast.program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'formal_list) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 32 "parser.mly"
  ( { fname = _2;
				formals = _3;
				body = Block(List.rev _5)})
# 310 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 38 "parser.mly"
      ([_1])
# 317 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'formal_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 39 "parser.mly"
                  (_2 :: _1 )
# 325 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 45 "parser.mly"
        ( [_1] )
# 332 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 46 "parser.mly"
                   ( _2 :: _1 )
# 340 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 49 "parser.mly"
       (Expr(_1))
# 347 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'stmt_list) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 50 "parser.mly"
                                                 ( If(_2, Block(List.rev _4), Block(List.rev _6)))
# 356 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 54 "parser.mly"
      ( Id(_1) )
# 363 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 55 "parser.mly"
                  (Binop (_1, Plus, _3) )
# 371 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 56 "parser.mly"
                   (Binop (_1, Minus, _3) )
# 379 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 57 "parser.mly"
                   (Binop (_1, Times, _3) )
# 387 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 58 "parser.mly"
                 (Binop (_1, Divide, _3) )
# 395 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 59 "parser.mly"
                 (Binop (_1, Mod, _3) )
# 403 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 60 "parser.mly"
                      (Binop(_1, Equality, _3))
# 411 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 61 "parser.mly"
                   (Binop(_1, Gthan, _3))
# 419 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 62 "parser.mly"
                   (Binop(_1, Lthan, _3))
# 427 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 63 "parser.mly"
                  (Binop(_1, Nsub, _3))
# 435 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 64 "parser.mly"
                    (Binop(_1, Nequal, _3))
# 443 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 65 "parser.mly"
                 ( Binop(_1, And, _3))
# 451 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 66 "parser.mly"
            ( Binop(_2, Not, Noexpr))
# 458 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 67 "parser.mly"
                ( Binop(_1, Or, _3))
# 466 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 68 "parser.mly"
                   ( Assign(_1, _3))
# 474 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 70 "parser.mly"
           ( Literal(_1) )
# 481 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 71 "parser.mly"
       ( Str(_1) )
# 488 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'optionals_list) in
    Obj.repr(
# 74 "parser.mly"
                                ( Set(_2) )
# 495 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_list) in
    Obj.repr(
# 75 "parser.mly"
                                 (Call(_1, List.rev _3))
# 503 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 78 "parser.mly"
  ([])
# 509 "parser.ml"
               : 'optionals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 79 "parser.mly"
                (_1)
# 516 "parser.ml"
               : 'optionals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 82 "parser.mly"
        ( [_1] )
# 523 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 83 "parser.mly"
                     ( _2 :: _1 )
# 531 "parser.ml"
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
