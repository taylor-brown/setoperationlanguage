let print = false

let _ =
	let ic = open_in "tests/test-assign.sol" in
	(*let lexbuf = Lexing.from_channel ic in*) 
	
	let lexbuf = Lexing.from_channel stdin in
	let program = Parser.program Scanner.token lexbuf in
	(*try*)
		if print then
		
			let listing = Solprinter.string_of_program program in
		print_string listing
	
	else
		
			ignore(Interpret.run(program))
		(*with exn ->
				begin
					let curr = lexbuf.Lexing.lex_curr_p in
					let line = curr.Lexing.pos_lnum in
					let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
					let tok = Lexing.lexeme lexbuf in
					print_endline("parse error:\nline " ^ string_of_int line ^
							"\ncnum " ^ string_of_int cnum ^
							"\ntoken " ^ tok)
				end
				*)