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
			| Assign(id, expr) -> if (NameMap.mem id env) then
						raise(Failure "cannot reassign variable")
					else
						let (_, exprval) = eval env expr in NameMap.add id exprval env, exprval
			| Str(str) -> env, Str(str)
			| Literal(lit) -> env, Literal(lit)
			| Bool(bl) -> env, Bool(bl)
			| Id(id) -> if NameMap.mem id env then 
					env, NameMap.find id env
				else if NameMap.mem id func_decls then 
						env, Func(NameMap.find id func_decls) 
					else raise(Failure ("unknown variable " ^ id))
			| Call("sizeof", set) -> env, (match set with
				| [Set(set)] -> Literal( List.length set)
				| _ -> raise(Failure "sizeof argument must be single set"))
			| Call("pop", set) -> env, (match set with
				| [Set(set)] -> List.hd set
				| _ -> raise(Failure "pop argument must be single set"))
			| Call("push", set) -> env, (match set with
				(* bug here *)
				| (Set(mset) :: arg) -> if List.length arg = 1 then Set(List.hd arg :: mset) else  raise(Failure "push arguments must be a set and an expression"))
			| Call("print", toprint) -> List.map (fun mprint -> print_endline(Solprinter.string_of_expr (snd(eval env mprint)))) toprint; env, Set([Noexpr])
			| Call("map", toprint) ->  raise(Failure "map not impemented")
			| Call("filter", toprint) ->  raise(Failure "filter not implemented")  
			| Call(id, cargs) -> if NameMap.mem id func_decls then 
						let margs = List.map (fun item -> let (_, mval) = eval env item in mval) cargs
						in env, (call (NameMap.find id func_decls) margs)
					else raise(Failure "unknown function name")
			| Set(set) -> let mset = List.map (fun item -> let (_, mval) = eval env item in mval) set
					in env, Set(mset)
			| Func(mfunc) -> env, Func mfunc
			| Binop(ex1, op, ex2) ->
					let (_, expr1) = eval env ex1 in
					let (_, expr2) = eval env ex2 in
					env,
					(match op with
						| Plus -> (match expr1, expr2 with
									| Literal e1, Literal e2 -> Literal(e1 + e2)
									| Str(e1), Str(e2) -> Str( e1 ^ e2)
									| Set(e1), Set(e2) -> Set( e1 @ e2)
									| _ -> raise(Failure("invalid types for plus")))
						| Minus -> (match expr1, expr2 with
									| Literal e1, Literal e2 -> Literal(e1 - e2)
									| Set(e1), Set(e2) -> raise(Failure("set difference not yet implemented"))
									| _ -> raise(Failure("invalid types for minus")))
						| Times -> (match expr1, expr2 with
									| Literal e1, Literal e2 -> Literal(e1 * e2)
									| Set(e1), Set(e2) -> raise(Failure("set cartesian product not yet implemented"))
									| _ -> raise(Failure("invalid types for times")))
						| Divide -> (match expr1, expr2 with
									| Literal e1, Literal e2 -> Literal(e1 / e2)
									| _ -> raise(Failure("invalid types for division")))
						| Mod -> (match expr1, expr2 with
									| Literal e1, Literal e2 -> Literal(e1 mod e2)
									| _ -> raise(Failure("invalid types for mod")))
						| Equality -> (match expr1, expr2 with
									| Literal e1, Literal e2 -> Bool(e1 = e2)
									| Str e1, Str e2 -> Bool(e1 = e2)
									| Set e1, Set e2 -> Bool(List.for_all (fun item -> List.mem item e2) e1)
									| _ -> raise(Failure("invalid types for equality")))
						| And -> (match expr1, expr2 with
									| Bool e1, Bool e2 -> Bool(e1 & e2)
									| Set e1, Set e2 -> raise(Failure("union of set not implemented"))
									| _ -> raise(Failure("invalid types for and")))
						| Not -> (match expr1 with
									| Bool e1 -> Bool(not e1)
									| _ -> raise(Failure("invalid types for not")))
						| Or -> (match expr1, expr2 with
									| Bool e1, Bool e2 -> Bool(e1 or e2)
									| _ -> raise(Failure("invalid types for or")))
						| Gthan -> (match expr1, expr2 with
									| Literal e1, Literal e2 -> Bool(e1 > e2)
									| Set e1, Set e2 -> raise(Failure("superset not implemented"))
									| _ -> raise(Failure("invalid types for >")))
						| Lthan -> (match expr1, expr2 with
									| Literal e1, Literal e2 -> Bool(e1 < e2)
									| Set e1, Set e2 -> raise(Failure("subset not implemented"))
									| _ -> raise(Failure("invalid types for <")))
						| Nsub -> (match expr1, expr2 with
									| Set e1, Set e2 -> raise(Failure("disjoint set not implemented"))
									| _ -> raise(Failure("invalid types for disjoint set")))
						| Nequal -> (match expr1, expr2 with
									| Bool e1, Bool e2 -> Bool(e1 != e2)
									| Literal e1, Literal e2 -> Bool(e1 != e2)
									| Str e1, Str e2 -> Bool(e1 != e2)
									| _ -> raise(Failure("invalid types for not equal"))))
		
		(* | Set(elements) -> (*trying to make this return a set, which itself *)
		(* is a list of Ast.expr. not working.                                 *)
		(Set(List.map (fun exprval locals -> exprval) (List.map (fun element -> eval locals element) elements)), locals) *)
		(*| Assign(id, expr) -> let (exprval,_) = eval locals expr
		in
		(exprval, NameMap.add id exprval locals) *)
		in
		let rec iftest execlocals test b1 b2 = let(_, result) = (eval execlocals test)in match result with
			| Bool(rst) ->(*print_endline(Solprinter.string_of_stmt b1);*) if rst then b1 else b2
			| _ -> raise(Failure "can only compare boolean relationships")
		and
		exec execlocals stmt = match stmt with
			| Block stmts -> let (local, result) = exec execlocals (List.hd stmts) in
					if (List.length stmts) = 1 then local, result
					else exec local (Block(List.tl stmts))
			| Expr expr -> eval execlocals expr
			| If(test, b1, b2) -> exec execlocals (iftest execlocals test b1 b2)
		(*| [Expr expr] -> eval locals expr
		| Expr hd :: stmt -> let (local, _) = eval locals hd in exec local stmt
		(*| Stmt hd :: stmt -> let (local, _) = exec locals hd in exec local stmt*)
		| [If(test, b1, b2)] -> iftest test b1 b2
		| If(test, b1, b2) :: stmt -> let(local, _) = iftest test b1 b2 in exec local stmt *)
		in
		snd (exec locals fdecl.body)
	in
	print_endline(Solprinter.string_of_expr(call (List.hd funcs) [Noexpr]))