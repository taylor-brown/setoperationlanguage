open Ast

let rec eval = function
	| Noexpr -> 1
	| Literal(i) -> i
	| _  -> print_endline("valid."); 1


	 
(*module NameMap = Map.Make( struct         *)
(*	type t = string                         *)
(*	let compare x y = Pervasives.compare x y*)
(*end)                                      *)
(*                                                *)
(*exception ReturnException of int * int NameMap.t*)

(*entry point *)

(*let run (vars, funcs) =                                       *)
(*	let func_decls = List.fold_left                             *)
(*			(fun funcs fdecl -> NameMap.add fdecl.fname fdecl funcs)*)
(*			NameMap.empty funcs                                     *)
(*	in                                                          *)
(*	                                                            *)
(*	let rec call fdecl actuals globals =                        *)
(*		let rec eval env = function                               *)
(*			  Literal(i) -> i, env                                  *)
(*			| Noexpr -> 1, env                                      *)
(*			| Id(var) -> 1, env                                     *)
(*			| Binop(e1, op, e2) -> 1, env                           *)
(*			| Assign(var, e) -> 1, env                              *)
(*			| Call("print", [e]) ->                                 *)
(*				let v, env = eval env e in                            *)
(*					print_endline(string_of_int v);                     *)
(*				0, env                                                *)
(*			| Call(f, actuals) -> 0, env                            *)
(*	  in                                                        *)
(*		let rec exec env = function                               *)
(*			  Block(stmts) -> List.fold_left exec env stmts         *)
(*			| _ -> 0                                                *)
(*			                                                        *)
(*		in                                                        *)
(*		print_endline("more crud?")                               *)
(*		in                                                        *)
(*		print_endline("even more")                                *)
		
