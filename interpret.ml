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
	(* not the most elegant set solution, but this isn't going to run google...*)
	let rec uniqueList = function
			| [] -> []
			| hd :: tl -> if List.mem hd tl then uniqueList tl else hd :: uniqueList tl
	in
	let rec setify inset = let sets = List.map (fun item -> match item with Set(set) -> set |_ -> raise(Failure"error"))
				 (List.filter (fun item -> match item with	| Set(item) -> true	| _ -> false) inset) in
			let nonsets = List.filter (fun item -> match item with | Set(item) -> false	| _ -> true) inset in
			( uniqueList(List.sort Pervasives.compare nonsets) @ (uniqueList (List.map (fun item -> Set(setify item)) sets)))
	in
	let rec call fdecl args =
		let locals = match args with
			| [] -> NameMap.empty
			| [Noexpr] -> NameMap.empty
			| _ -> List.fold_left2 (fun locals formal actual -> NameMap.add formal actual locals) NameMap.empty fdecl.formals args
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
			| Call("push", set) ->let margs = List.map (fun item -> let (_, mval) = eval env item in mval) set
				(*in let margs = setify margs*)
				in env, (match margs with
 				| [Set(mset) ; arg] -> Set(setify (arg :: mset))
				| _ -> raise(Failure "push arguments must be a set and an expression"))
			| Call("print", toprint) -> List.iter (fun mprint -> print_endline(Solprinter.string_of_expr (snd(eval env mprint)))) toprint; env, Set([Noexpr])
			| Call("map", cargs) ->  let margs = List.map (fun item -> let (_, mval) = eval env item in mval) cargs in
				env, (match margs with 
					| [Func(id); Set(mset)] -> let func = if NameMap.mem id.fname func_decls then NameMap.find id.fname func_decls else raise(Failure "function not found ") in
						Set(setify(List.map (fun item -> call func [item]) mset)) 
					| _  -> raise(Failure "map must be called with a function and a set"))
			| Call("apply", args) ->
				env, (match args with
 				| Func(id) :: arg -> if NameMap.mem id.fname func_decls then 
						(call (NameMap.find id.fname func_decls) arg)
					else raise(Failure "unknown function name")
				| _ -> raise(Failure "apply arguments must be a function id and arguments"))
			| Call("filter", cargs) ->    let margs = List.map (fun item -> let (_, mval) = eval env item in mval) cargs in
				env, (match margs with 
					| [Func(id); Set(mset)] -> let func = if NameMap.mem id.fname func_decls then NameMap.find id.fname func_decls else raise(Failure "function not found ") in
						Set(setify(List.filter (fun item -> match call func [item]with |Bool(result) -> result | _ -> raise(Failure "filter function must return bool")) mset)) 
					| _  -> raise(Failure "map must be called with a function and a set"))
			| Call(id, cargs) -> if NameMap.mem id func_decls then
						let margs = List.rev ( List.map (fun item -> let (_, mval) = eval env item in mval) cargs)
						in env, (call (NameMap.find id func_decls) margs)
					else raise(Failure "unknown function name")
			| Set(set) -> let mset = List.map (fun item -> let (_, mval) = eval env item in mval) set
					in env, Set(setify mset)
			| Func(mfunc) -> env, Func mfunc
			| Binop(ex1, op, ex2) ->
					let (_, expr1) = eval env ex1 in
					let (_, expr2) = eval env ex2 in
					env,
					(match op with
						| Plus -> (match expr1, expr2 with
									| Literal e1, Literal e2 -> Literal(e1 + e2)
									| Str(e1), Str(e2) -> Str( e1 ^ e2)
									| Set(e1), Set(e2) -> Set(setify( e1 @ e2))
									| _ -> raise(Failure("invalid types for plus")))
						| Minus -> (match expr1, expr2 with
									| Literal e1, Literal e2 -> Literal(e1 - e2)
									| Set(e1), Set(e2) -> Set(List.filter (fun item -> not( List.mem item e2)) e1)  
									| _ -> raise(Failure("invalid types for minus")))
						| Times -> (match expr1, expr2 with
									| Literal e1, Literal e2 -> Literal(e1 * e2)
									| Set(e1), Set(e2) -> Set(setify (List.fold_left (fun t x -> (List.fold_left (fun y z -> Set([x; z])::y)[] e1)@ t)[] e2))
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
									| Bool e1, Bool e2 -> Bool(e1 = e2)
									| _ -> raise(Failure("invalid types for equality")))
						| And -> (match expr1, expr2 with
									| Bool e1, Bool e2 -> Bool(e1 & e2)
									| Set e1, Set e2 ->Set(List.filter (fun item -> List.mem item e2) e1)
									| _ -> raise(Failure("invalid types for and")))
						| Not -> (match expr1 with
									| Bool e1 -> Bool(not e1)
									| _ -> raise(Failure("invalid types for not")))
						| Or -> (match expr1, expr2 with
									| Bool e1, Bool e2 -> Bool(e1 or e2)
									| _ -> raise(Failure("invalid types for or")))
						| Gthan -> (match expr1, expr2 with
									| Literal e1, Literal e2 -> Bool(e1 > e2)
									| Set e1, Set e2 -> Bool(List.length (List.filter (fun item -> List.mem item e2) e1) >= (List.length e2))
									| _ -> raise(Failure("invalid types for >")))
						| Lthan -> (match expr1, expr2 with
									| Literal e1, Literal e2 -> Bool(e1 < e2)
									| Set e1, Set e2 -> Bool(List.length (List.filter (fun item -> List.mem item e2) e1) <= (List.length e1))
									| _ -> raise(Failure("invalid types for <")))
						| Nsub -> (match expr1, expr2 with
									| Set e1, Set e2 ->Bool(List.length (List.filter (fun item -> List.mem item e2) e1) = 0)
									| _ -> raise(Failure("invalid types for disjoint set")))
						| Nequal -> (match expr1, expr2 with
									| Bool e1, Bool e2 -> Bool(e1 != e2)
									| Literal e1, Literal e2 -> Bool(e1 != e2)
									| Str e1, Str e2 -> Bool(e1 != e2)
									| _ -> raise(Failure("invalid types for not equal"))))
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
		in
		snd (exec locals fdecl.body)
	in
	try
	print_endline(Solprinter.string_of_expr(call (List.hd funcs) [Noexpr]))
	with exn -> match exn with
		| Failure(text) -> print_endline text
		| _ -> raise exn