let print = false

let _ =
(*let ic = open_in "tests/test-binops.sol" in*)
(* print_endline("debug mode");              *)
(*	let lexbuf = Lexing.from_channel ic in   *)
	(*let stringAutos = 
		""  in
	let lexbufAutos  = Lexing.from_string stringAutos in
	let programAutos = Parser.program Scanner.token lexbufAutos in*)
	let lexbuf = Lexing.from_channel stdin in
	try
		let program = Parser.program Scanner.token lexbuf in

		if print then
		
			let listing = Solprinter.string_of_program program in
		print_string listing
	
	else
		
			ignore(Interpret.run(program))

			with exn -> match exn with
				| Parsing.Parse_error ->
				begin
					let curr = lexbuf.Lexing.lex_curr_p in
					let line = curr.Lexing.pos_lnum in
					let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
					let tok = Lexing.lexeme lexbuf in
					print_endline("parse error:\nline " ^ string_of_int line ^
							"\ncnum " ^ string_of_int cnum ^
							"\ntoken " ^ tok)
				end
				|_ -> raise(exn)