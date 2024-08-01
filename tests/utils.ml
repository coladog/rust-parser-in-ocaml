open Lexer
open Syntax 
open Str

let preprocess_if_expr str = 
	let strs_sep_if = Str.full_split (Str.regexp "\\bif\\b") str in
	let ret = ref "" in 
	let met_delim = ref false in 
	(* return original string if not matching if *)
	let rec loop strs_sep_if = 
		match strs_sep_if with
		| [] -> !ret
		| hd::tl -> 
			match hd with 
			| Delim _ -> 
				met_delim := true;
				loop tl
			| Text s -> 
				if not !met_delim then 
					loop tl
					(* match if the next word is let, if it is, then skip *)
				else if Str.string_match (Str.regexp "\\blet\\b") s 0 then 
					loop tl
				else begin
					ret := !ret ^ "if(";
					let str_arr = Array.of_seq (String.to_seq s) in
					(* find the first occurance of { when paren_cnt is 0, put a ) *)
					let paren_cnt = ref 0 in
					let inserted_rparen = ref false in
					for i = 0 to Array.length str_arr - 1 do
						if str_arr.(i) = '(' then paren_cnt := !paren_cnt + 1
						else if str_arr.(i) = ')' then paren_cnt := !paren_cnt - 1
						else if not !inserted_rparen
								 && str_arr.(i) = '{' && !paren_cnt = 0 then begin
							ret := !ret ^ ")";
							inserted_rparen := true
						end;
						ret := !ret ^ (String.make 1 str_arr.(i)) 
					done;

					met_delim := false;
					loop tl
				end 
	in
	loop strs_sep_if

let parse_ast root_element str = 
	let lexbuf = Lexing.from_string str in
	let ast = root_element Lexer.read_token lexbuf in
	ast  

let item_testable = Alcotest.testable Syntax.pp_item Syntax.equal_item
let fn_sig_testable = Alcotest.testable Syntax.pp_fn_sig Syntax.equal_fn_sig
let int_lit_testable = Alcotest.testable Syntax.pp_integer_literal Syntax.equal_integer_literal
let op_expr_testable = Alcotest.testable Syntax.pp_op_expr Syntax.equal_op_expr
(* let literal_expr_testable = Alcotest.testable Syntax.pp_literal_expr Syntax.equal_literal_expr *)
let expr_testable = Alcotest.testable Syntax.pp_expr Syntax.equal_expr
let array_expr_testable = Alcotest.testable Syntax.pp_array_expr Syntax.equal_array_expr
let index_expr_testable = Alcotest.testable Syntax.pp_tuple_index_expr Syntax.equal_tuple_index_expr
let struct_expr_testable = Alcotest.testable Syntax.pp_struct_expr Syntax.equal_struct_expr