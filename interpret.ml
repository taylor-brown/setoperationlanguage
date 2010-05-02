open Ast

module NameMap = Map.Make(struct
		type t = string
		let compare x y = Pervasives.compare x y
	end)

(* exception ReturnException of int * int NameMap.t *)

(* Main entry point: run a program *)

let run (funcs) =
	let func_decls = List.fold_left
			(fun funcs fdecl -> NameMap.add fdecl.fname fdecl funcs)
			NameMap.empty funcs
	in
	let rec call fdecl args =
		let locals =
			List.fold_left2 (fun locals formal actual -> NameMap.add formal actual locals) NameMap.empty fdecl.formals args
		in
		let rec eval env expr = match expr with
			| Noexpr -> env, Set([Noexpr])
			| Assign(id, expr) ->if (NameMap.mem id locals) then
				raise(Failure "cannot reassign variable")
				else
			  let (_, exprval) = eval env expr in NameMap.add id exprval env, exprval
			| Str(str) -> env, Str(str)
			| Literal(lit) -> env, Literal(lit)
			| Bool(bl) -> env, Bool(bl)
			| Id(id) -> if NameMap.mem id env then env, NameMap.find id env 
				else if NameMap.mem id func_decls then env, Func(NameMap.find id func_decls) else 
					raise(Failure "unknown variable")
			| Call(id, cargs) -> if NameMap.mem id func_decls then 
					env, (call (NameMap.find id func_decls) cargs) 
				else raise(Failure "unknown function name") 
			| Set(set) -> let mset = List.map (fun item  -> let (_, mval) = eval env item in mval) set 
				in env, Set(mset) 
			| Func(mfunc) -> env, Func mfunc
			| Binop(ex1, op, ex2) -> 
				let (_, expr1) = eval env ex1 in
				let (_, expr2) = eval env ex2 in
				let boolean i = if i then true else false
					in
					env,
					(match op with
						| Plus  -> (match expr1,expr2 with
							| Literal e1, Literal e2  -> Literal(e1 + e2)
							| Str(e1), Str(e2)  -> Str( e1 ^ e2)
							| Set(e1), Set(e2) -> Set( e1 @ e2)
							| _ -> raise(Failure("invalid types for plus")))
						| Minus -> (match expr1, expr2 with
							| Literal e1, Literal e2  -> Literal(e1 - e2)
							| Set(e1), Set(e2) -> raise(Failure("set difference not yet implemented"))
							| _ -> raise(Failure("invalid types for minus")))
						| Times ->  (match expr1, expr2 with
							| Literal e1, Literal e2  -> Literal(e1 * e2)
							| Set(e1), Set(e2) -> raise(Failure("set cartesian product not yet implemented"))
							| _ -> raise(Failure("invalid types for times")))
						| Divide -> (match expr1, expr2 with
							| Literal e1, Literal e2  -> Literal(e1 / e2)
							| _ -> raise(Failure("invalid types for division")))
						| Mod -> (match expr1, expr2 with
							| Literal e1, Literal e2  -> Literal(e1 mod e2)
							| _ -> raise(Failure("invalid types for mod")))
						| Equality ->(match expr1, expr2 with
							| Literal e1, Literal e2  -> Bool(e1 = e2)
							| Str e1, Str e2 -> Bool(e1 = e2)
							| Set e1, Set e2 -> Bool(List.for_all (fun item -> List.mem item e2) e1) 
							| _ -> raise(Failure("invalid types for equality")))
						| And ->  (match expr1, expr2 with
							| Bool e1, Bool e2  -> Bool(e1 & e2)
							| Set e1, Set e2 -> raise(Failure("union of set not implemented"))
							| _ -> raise(Failure("invalid types for and")))
						| Not ->   (match expr1 with
							| Bool e1  -> Bool(not e1)
							| _ -> raise(Failure("invalid types for not")))
						| Or ->   (match expr1, expr2 with
							| Bool e1, Bool e2  -> Bool(e1 or e2)
							| _ -> raise(Failure("invalid types for or")))
						| Gthan ->   (match expr1, expr2 with
							| Literal e1, Literal e2  -> Bool(e1 > e2)
							| Set e1, Set e2 -> raise(Failure("superset not implemented"))
							| _ -> raise(Failure("invalid types for >")))
						| Lthan ->  (match expr1, expr2 with
							| Literal e1, Literal e2  -> Bool(e1 < e2)
							| Set e1, Set e2 -> raise(Failure("subset not implemented"))
							| _ -> raise(Failure("invalid types for <")))
						| Nsub ->  (match expr1, expr2 with
							| Set e1, Set e2 -> raise(Failure("disjoint set not implemented"))
							| _ -> raise(Failure("invalid types for disjoint set")))
						| Nequal ->  (match expr1, expr2 with
							| Bool e1, Bool e2  -> Bool(e1 != e2)
							| Literal e1, Literal e2  -> Bool(e1 = e2)
							| Str e1, Str e2 -> Bool(e1 != e2)
							| _ -> raise(Failure("invalid types for not equal"))))

		(* | Set(elements) -> (*trying to make this return a set, which itself is  *)
		(* a list of Ast.expr. not working.                                        *)
		(Set(List.map (fun exprval locals -> exprval) (List.map (fun element -> eval locals element) elements)), locals) *)
		(*| Assign(id, expr) -> let (exprval,_) = eval locals expr
		in
		(exprval, NameMap.add id exprval locals) *)
		in
		let rec iftest test b1 b2 =let(_,result) = (eval locals test)in match result with
				| Bool(rst) -> if rst then exec locals b1 else exec locals b2
				| _ -> raise(Failure "can only compare boolean relationships")
		and
		exec locals stmts = match stmts with
			| [Expr expr] -> eval locals expr
			| Expr hd :: stmt -> let (local, _) = eval locals hd in exec local stmt 
			(*| Stmt hd :: stmt -> let (local, _) = exec locals hd in exec local stmt*) 
			| [If(test, b1, b2)] -> iftest test b1 b2
			| If(test, b1, b2) ::stmt -> let(local,_) = iftest test b1 b2 in exec local stmt 
			| _ -> raise(Failure "statement list escape")
		in
		snd (exec locals fdecl.body)
	in
	print_endline(Solprinter.string_of_expr(call (List.hd funcs) [Noexpr]))
(*	match (call (List.hd funcs) [Noexpr]) with
		| Str(mval) -> print_endline(mval)
		| Literal(someval) -> print_endline(string_of_int someval)
		| ReturnValue(rtrn) -> print_endline("return type return")
		| _ -> print_endline("return value type not yet implemented")*)

(* in eval(0,List.hd(List.rev funcs.body)) Put function declarations in a  *)
(* symbol table let func_decls = List.fold_left (fun funcs fdecl ->        *)
(* NameMap.add fdecl.fname fdecl funcs) NameMap.empty funcs in (* Invoke a *)
(* function and return an updated global symbol table                      *)
let rec call fdecl actuals globals =

(* Evaluate an expression and return (value, updated environment) *)
let rec eval env = function
Literal(i) -> i, env
| Noexpr -> 1, env (* must be non-zero for the for loop predicate *)
| Id(var) ->
let locals, globals = env in
if NameMap.mem var locals then
(NameMap.find var locals), env
else if NameMap.mem var globals then
(NameMap.find var globals), env
else raise (Failure ("undeclared identifier " ^ var))
| Binop(e1, op, e2) ->
let v1, env = eval env e1 in
let v2, env = eval env e2 in
let boolean i = if i then 1 else 0 in
(match op with
Add -> v1 + v2
| Sub -> v1 - v2
| Mult -> v1 * v2
| Div -> v1 / v2
| Equal -> boolean (v1 = v2)
| Neq -> boolean (v1 != v2)
| Less -> boolean (v1 < v2)
| Leq -> boolean (v1 <= v2)
| Greater -> boolean (v1 > v2)
| Geq -> boolean (v1 >= v2)), env
| Assign(var, e) ->
let v, (locals, globals) = eval env e in
if NameMap.mem var locals then
v, (NameMap.add var v locals, globals)
else if NameMap.mem var globals then
v, (locals, NameMap.add var v globals)
else raise (Failure ("undeclared identifier " ^ var))
| Call("print", [e]) ->
let v, env = eval env e in
print_endline (string_of_int v);
0, env
| Call(f, actuals) ->
let fdecl =
try NameMap.find f func_decls
with Not_found -> raise (Failure ("undefined function " ^ f))
in
let actuals, env = List.fold_left
(fun (actuals, env) actual ->
let v, env = eval env actual in v :: actuals, env)
([], env) actuals
in
let (locals, globals) = env in
try
let globals = call fdecl actuals globals in 0, (locals, globals)
with ReturnException(v, globals) -> v, (locals, globals)
in

(* Execute a statement and return an updated environment *)
let rec exec env = function
Block(stmts) -> List.fold_left exec env stmts
| Expr(e) -> let _, env = eval env e in env
| If(e, s1, s2) ->
let v, env = eval env e in
exec env (if v != 0 then s1 else s2)
| While(e, s) ->
let rec loop env =
let v, env = eval env e in
if v != 0 then loop (exec env s) else env
in loop env
| For(e1, e2, e3, s) ->
let _, env = eval env e1 in
let rec loop env =
let v, env = eval env e2 in
if v != 0 then
let _, env = eval (exec env s) e3 in
loop env
else
env
in loop env
| Return(e) ->
let v, (locals, globals) = eval env e in
raise (ReturnException(v, globals))
in

(* Enter the function: bind actual values to formal arguments *)
let locals =
try List.fold_left2
(fun locals formal actual -> NameMap.add formal actual locals)
NameMap.empty fdecl.formals actuals
with Invalid_argument(_) ->
raise (Failure ("wrong number of arguments passed to " ^ fdecl.fname))
in
(* Initialize local variables to 0 *)
let locals = List.fold_left
(fun locals local -> NameMap.add local 0 locals) locals fdecl.locals
in
(* Execute each statement in sequence, return updated global symbol    *)
(* table                                                               *)
snd (List.fold_left exec (locals, globals) fdecl.body)

(* Run a program: initialize global variables to 0, find and run "main" *)
in let globals = List.fold_left
(fun globals vdecl -> NameMap.add vdecl 0 globals) NameMap.empty vars
in try
call (NameMap.find "main" func_decls) [] globals
with Not_found -> raise (Failure ("did not find the main() function")) *)

(* let rec eval = function | Noexpr -> 1 | Literal(i) -> i | _ ->          *)
(* print_endline("valid."); 1                                              *)

(* module NameMap = Map.Make( struct type t = string let compare x y =     *)
(* Pervasives.compare x y end) exception ReturnException of int * int      *)
(* NameMap.t                                                               *)

(* entry point *)

(* let run (vars, funcs) = let func_decls = List.fold_left (fun funcs      *)
(* fdecl -> NameMap.add fdecl.fname fdecl funcs) NameMap.empty funcs in    *)
(* let rec call fdecl actuals globals = let rec eval env = function        *)
(* Literal(i) -> i, env | Noexpr -> 1, env | Id(var) -> 1, env | Binop(e1, *)
(* op, e2) -> 1, env | Assign(var, e) -> 1, env | Call("print", [e]) ->    *)
(* let v, env = eval env e in print_endline(string_of_int v); 0, env |     *)
(* Call(f, actuals) -> 0, env in let rec exec env = function Block(stmts)  *)
(* -> List.fold_left exec env stmts | _ -> 0 in print_endline("more        *)
(* crud?") in print_endline("even more")                                   *)
