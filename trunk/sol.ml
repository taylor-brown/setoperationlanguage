let print = false
let _ =
	let lexbuf = Lexing.from_channel stdin in
	let program = Parser.program Scanner.token lexbuf in
		ignore(Interpret.eval(program))