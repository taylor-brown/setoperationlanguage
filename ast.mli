type op = Plus | Minus | Times | Divide | Mod | Equality | And | Not | Or | Gthan | Lthan | Nsub | Nequal 
(*type boolean = true | false*)

type expr = 
		Literal of int
	| Id of string
	| Binop of expr * op * expr
	| Assign of string * expr
	| Call of string * expr list
	| Set of expr list
	| Str of string
	| Func of func_decl
	| Noexpr
and
 stmt = 
	  Block of stmt list
	| Expr of expr
	| If of expr * stmt * stmt
and
 func_decl = {
	fname : string;
	formals : string list;
	body : stmt list;
	}
	
type program = func_decl list