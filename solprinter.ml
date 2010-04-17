open Ast

let rec string_of_expr = function
	| Literal(l) -> string_of_int l
	| Id(s)  ->  s
	| Binop(e1, op, e2)  -> 
			string_of_expr e1 ^ " " ^
			(match op with
				Plus ->  "+"
				| Minus ->  "-" 
				| Times ->  "*"
				| Divide ->  "/"
				| Mod ->  "%"
				| Equality ->  "==" 
				| And ->  "&"
				| Not ->  "!"
				| Or ->  "|"
				| Gthan ->  ">"
				| Lthan ->  "<"
				| Nsub ->  "!<"
				| Nequal ->  "!=") ^ " " ^ string_of_expr e2
	| Assign(v,e)  ->  v ^ " = " ^ string_of_expr e
	| Call(f, el)  ->  f ^ "(" ^ String.concat ", " (List.rev (List.map string_of_expr el)) ^ ")"
	| Func(f)  -> "not implemented\n"
	| Str(s) -> s
	| Set(set) -> "some set..."
	| Noexpr -> ""

let rec string_of_stmt = function
	| Block(stmts) ->
		"\t" ^ String.concat "" (List.map string_of_stmt stmts) ^ "\n"
	| Expr(expr)  -> string_of_expr expr ^ "\n"
	| If(test, b1, b2)  -> "if (" ^ string_of_expr test ^ "):\n" ^
			string_of_stmt b1 ^ "else:\n" ^ string_of_stmt b2 ^ "end\n" 

let rec string_of_fdecl fdecl =
	"function " ^fdecl.fname ^ " " ^ String.concat "," (List.rev fdecl.formals) ^ "\n" ^
	String.concat "" (List.map string_of_stmt fdecl.body) ^ "\nend\n"

let string_of_program(funcs) =
	String.concat "\n" (List.map string_of_fdecl funcs) 