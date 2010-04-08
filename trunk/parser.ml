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
let parse_error s= print_endline s
 
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
\001\000\002\000\003\000\003\000\005\000\004\000\006\000\006\000\
\007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\008\000\008\000\000\000"

let yylen = "\002\000\
\001\000\006\000\001\000\003\000\001\000\002\000\002\000\006\000\
\001\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\001\000\004\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\029\000\001\000\000\000\003\000\000\000\
\000\000\000\000\000\000\004\000\000\000\000\000\002\000\025\000\
\000\000\006\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\007\000\024\000\000\000\000\000\
\000\000\000\000\000\000\000\000\012\000\013\000\014\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\026\000\000\000\000\000\008\000"

let yydgoto = "\002\000\
\004\000\005\000\008\000\011\000\000\000\018\000\019\000\041\000"

let yysindex = "\043\000\
\023\255\000\000\028\255\000\000\000\000\030\255\000\000\253\254\
\000\000\035\255\007\255\000\000\047\255\047\255\000\000\000\000\
\114\255\000\000\002\000\021\000\036\000\047\255\047\255\047\255\
\047\255\047\255\047\255\047\255\047\255\047\255\047\255\047\255\
\047\255\047\255\047\255\047\255\000\000\000\000\034\255\049\000\
\045\255\049\000\252\254\252\254\000\000\000\000\000\000\216\255\
\216\255\216\255\216\255\216\255\062\000\252\254\062\000\054\255\
\000\000\049\000\034\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\237\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\080\255\
\000\000\211\255\024\255\051\255\000\000\000\000\000\000\097\255\
\116\255\135\255\154\255\173\255\192\255\078\255\203\255\000\000\
\000\000\099\255\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\221\255\243\255\000\000"

let yytablesize = 336
let yytable = "\020\000\
\021\000\009\000\010\000\056\000\026\000\027\000\028\000\013\000\
\040\000\042\000\043\000\044\000\045\000\046\000\047\000\048\000\
\049\000\050\000\051\000\052\000\053\000\054\000\055\000\060\000\
\010\000\010\000\014\000\058\000\010\000\015\000\010\000\010\000\
\016\000\017\000\013\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\001\000\003\000\013\000\057\000\013\000\
\010\000\010\000\010\000\011\000\011\000\014\000\006\000\011\000\
\007\000\011\000\011\000\016\000\017\000\012\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\016\000\017\000\
\016\000\017\000\059\000\011\000\011\000\011\000\018\000\018\000\
\027\000\027\000\018\000\000\000\018\000\018\000\000\000\000\000\
\000\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
\018\000\020\000\020\000\028\000\028\000\020\000\018\000\018\000\
\018\000\027\000\027\000\000\000\020\000\020\000\020\000\020\000\
\020\000\020\000\022\000\020\000\021\000\021\000\000\000\000\000\
\021\000\020\000\020\000\020\000\028\000\028\000\000\000\021\000\
\021\000\021\000\021\000\021\000\021\000\000\000\021\000\022\000\
\022\000\023\000\000\000\022\000\021\000\021\000\021\000\000\000\
\000\000\000\000\022\000\022\000\022\000\022\000\022\000\022\000\
\000\000\022\000\017\000\017\000\000\000\000\000\017\000\022\000\
\022\000\022\000\000\000\000\000\000\000\017\000\017\000\017\000\
\017\000\017\000\017\000\000\000\017\000\016\000\016\000\000\000\
\000\000\016\000\017\000\017\000\017\000\000\000\000\000\000\000\
\016\000\016\000\016\000\016\000\016\000\016\000\000\000\016\000\
\015\000\015\000\000\000\000\000\015\000\016\000\016\000\016\000\
\000\000\000\000\000\000\019\000\019\000\000\000\000\000\019\000\
\015\000\000\000\015\000\023\000\023\000\000\000\000\000\023\000\
\015\000\015\000\015\000\019\000\000\000\019\000\024\000\025\000\
\026\000\027\000\028\000\019\000\019\000\019\000\000\000\000\000\
\000\000\035\000\000\000\023\000\023\000\023\000\009\000\000\000\
\000\000\009\000\000\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\000\000\000\000\000\000\000\000\000\000\009\000\009\000\009\000\
\024\000\025\000\026\000\027\000\028\000\029\000\030\000\031\000\
\032\000\033\000\034\000\035\000\036\000\000\000\038\000\000\000\
\000\000\000\000\037\000\024\000\025\000\026\000\027\000\028\000\
\029\000\030\000\031\000\032\000\033\000\034\000\035\000\036\000\
\039\000\000\000\024\000\025\000\026\000\027\000\028\000\029\000\
\030\000\031\000\032\000\033\000\034\000\035\000\036\000\024\000\
\025\000\026\000\027\000\028\000\029\000\030\000\031\000\032\000\
\033\000\034\000\035\000\036\000\024\000\025\000\026\000\027\000\
\028\000\029\000\030\000\031\000\032\000\033\000\000\000\035\000"

let yycheck = "\013\000\
\014\000\005\001\006\001\039\000\009\001\010\001\011\001\001\001\
\022\000\023\000\024\000\025\000\026\000\027\000\028\000\029\000\
\030\000\031\000\032\000\033\000\034\000\035\000\036\000\059\000\
\001\001\002\001\020\001\041\000\005\001\023\001\007\001\008\001\
\026\001\027\001\001\001\012\001\013\001\014\001\015\001\016\001\
\017\001\018\001\019\001\001\000\022\001\001\001\002\001\001\001\
\025\001\026\001\027\001\001\001\002\001\020\001\027\001\005\001\
\027\001\007\001\008\001\026\001\027\001\027\001\012\001\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\026\001\027\001\
\026\001\027\001\021\001\025\001\026\001\027\001\001\001\002\001\
\001\001\002\001\005\001\255\255\007\001\008\001\255\255\255\255\
\255\255\012\001\013\001\014\001\015\001\016\001\017\001\018\001\
\019\001\001\001\002\001\001\001\002\001\005\001\025\001\026\001\
\027\001\026\001\027\001\255\255\012\001\013\001\014\001\015\001\
\016\001\017\001\001\001\019\001\001\001\002\001\255\255\255\255\
\005\001\025\001\026\001\027\001\026\001\027\001\255\255\012\001\
\013\001\014\001\015\001\016\001\017\001\255\255\019\001\001\001\
\002\001\024\001\255\255\005\001\025\001\026\001\027\001\255\255\
\255\255\255\255\012\001\013\001\014\001\015\001\016\001\017\001\
\255\255\019\001\001\001\002\001\255\255\255\255\005\001\025\001\
\026\001\027\001\255\255\255\255\255\255\012\001\013\001\014\001\
\015\001\016\001\017\001\255\255\019\001\001\001\002\001\255\255\
\255\255\005\001\025\001\026\001\027\001\255\255\255\255\255\255\
\012\001\013\001\014\001\015\001\016\001\017\001\255\255\019\001\
\001\001\002\001\255\255\255\255\005\001\025\001\026\001\027\001\
\255\255\255\255\255\255\001\001\002\001\255\255\255\255\005\001\
\017\001\255\255\019\001\001\001\002\001\255\255\255\255\005\001\
\025\001\026\001\027\001\017\001\255\255\019\001\007\001\008\001\
\009\001\010\001\011\001\025\001\026\001\027\001\255\255\255\255\
\255\255\018\001\255\255\025\001\026\001\027\001\002\001\255\255\
\255\255\005\001\255\255\007\001\008\001\009\001\010\001\011\001\
\012\001\013\001\014\001\015\001\016\001\017\001\018\001\019\001\
\255\255\255\255\255\255\255\255\255\255\025\001\026\001\027\001\
\007\001\008\001\009\001\010\001\011\001\012\001\013\001\014\001\
\015\001\016\001\017\001\018\001\019\001\255\255\002\001\255\255\
\255\255\255\255\025\001\007\001\008\001\009\001\010\001\011\001\
\012\001\013\001\014\001\015\001\016\001\017\001\018\001\019\001\
\005\001\255\255\007\001\008\001\009\001\010\001\011\001\012\001\
\013\001\014\001\015\001\016\001\017\001\018\001\019\001\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\007\001\008\001\009\001\010\001\
\011\001\012\001\013\001\014\001\015\001\016\001\255\255\018\001"

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
# 37 "parser.mly"
         ( _1 )
# 253 "parser.ml"
               :  Ast.program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'formal_list) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 41 "parser.mly"
  ( { fname = _2;
				formals = _3;
				body = List.rev _5})
# 264 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 46 "parser.mly"
     ([_1])
# 271 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 47 "parser.mly"
                        (_3 :: _1 )
# 279 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 50 "parser.mly"
    ( _1 )
# 286 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 54 "parser.mly"
                  ( _2 :: _1 )
# 294 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 57 "parser.mly"
           (Expr(_1))
# 301 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 58 "parser.mly"
                                ( If(_2, _4, _6))
# 310 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 61 "parser.mly"
      ( Id(_1) )
# 317 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 62 "parser.mly"
                  (Binop (_1, Plus, _3) )
# 325 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 63 "parser.mly"
                   (Binop (_1, Minus, _3) )
# 333 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 64 "parser.mly"
                   (Binop (_1, Times, _3) )
# 341 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 65 "parser.mly"
                 (Binop (_1, Divide, _3) )
# 349 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 66 "parser.mly"
                 (Binop (_1, Mod, _3) )
# 357 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 67 "parser.mly"
                      (Binop(_1, Equality, _3))
# 365 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 68 "parser.mly"
                   (Binop(_1, Gthan, _3))
# 373 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 69 "parser.mly"
                   (Binop(_1, Lthan, _3))
# 381 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 70 "parser.mly"
                  (Binop(_1, Nsub, _3))
# 389 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 71 "parser.mly"
                    (Binop(_1, Nequal, _3))
# 397 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 72 "parser.mly"
                 ( Binop(_1, And, _3))
# 405 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 73 "parser.mly"
                 ( Binop(_1, Not, _3))
# 413 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 74 "parser.mly"
                ( Binop(_1, Or, _3))
# 421 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 75 "parser.mly"
                   ( Assign(_1, _3))
# 429 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 76 "parser.mly"
                      (_2)
# 436 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 77 "parser.mly"
           ( Literal(_1) )
# 443 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_list) in
    Obj.repr(
# 81 "parser.mly"
                                 (Call(_1, _3))
# 451 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 89 "parser.mly"
        ( [_1] )
# 458 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 90 "parser.mly"
                     ( _2 :: _1 )
# 466 "parser.ml"
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
