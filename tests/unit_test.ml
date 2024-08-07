open Utils
open RSsyntax 
open RSparser


let test_struct_def1 () = 
	(* with visibility params and fields *)
	let parse_ast = parse_ast false item_toplevel in
	let ipt_str = "
	pub struct Point {
		x: i32,
		y: i32,
	}" in
	let field1 = (None, None, "x", "i32") in
	let field2 = (None, None, "y", "i32") in
	let fields = Some [field1; field2] in
	let struct_def = ("Point", None, None, fields) in
	let struct_item = Struct struct_def in
	let expected = Vis_Item (None, Some Pub, Struct_Vis_Item struct_item) in
	Alcotest.(check item_testable) "same item" expected (parse_ast ipt_str);

	let ipt_str = "
	pub struct Point {
		x: i32,
		y: i32
	}" in (* should be the same without the comma *)
	Alcotest.(check item_testable) 
		"same item without comma for fields" expected (parse_ast ipt_str);

	let ipt_str = "
	pub struct Point {
		x: i32,
		y: i32
	};" in (* should be the same with the semicolon *)

	Alcotest.(check item_testable) 
		"same item with semicolon" expected (parse_ast ipt_str);

	let ipt_str = "
	struct Point{
		x: i32  
		y: i32
	}
	" in (* should raise exception since forgot to add comma *)
	Alcotest.check_raises 
		"missing comma" (RSparser.Error) (fun () -> let _ = parse_ast ipt_str in ())

let test_struct_def2 () = 
	(* unit structs *)
	let parse_ast = parse_ast false item_toplevel in
	let ipt_str = "
	pub crate struct Point;
	" in 
	let fields = None in
	let struct_def = ("Point", None, None, fields) in
	let struct_item = Struct struct_def in
	let expected = Vis_Item (None, Some Pub_Crate, Struct_Vis_Item struct_item) in
	Alcotest.(check item_testable) "unit struct" expected (parse_ast ipt_str);  
	let ipt_str = "
	struct Point;
	" in
	let struct_name = "Point" in
	let struct_generic_params = None in
	let struct_where_clause = None in
	let struct_fields = None in

	let struct_def = (struct_name, struct_generic_params, struct_where_clause, struct_fields) in
	let struct_item = Struct struct_def in
	let expected = Vis_Item (None, None, Struct_Vis_Item struct_item)  in
	Alcotest.(check item_testable) 
		"unit struct without pub" expected (parse_ast ipt_str) 

let test_struct_def3 () = 
	(* tuple struct *)
	let parse_ast = parse_ast false item_toplevel in
	let ipt_str = " 
	pub struct Point(i32, i32);
	" in 
	(* Expected AST construction *)
	let tuple_field1 = (None, None, "i32") in
	let tuple_field2 = (None, None, "i32") in
	let tuple_fields = Some [tuple_field1; tuple_field2] in
	let tuple_def = ("Point", None, tuple_fields, None) in
	let struct_item = Tuple tuple_def in
	let expected = Vis_Item (None, Some Pub, Struct_Vis_Item struct_item) in
	Alcotest.(check item_testable) "tuple struct" expected (parse_ast ipt_str)

let test_str_lit1() = 
	(* string literal *)
	let parse_ast = parse_ast false string_literal_toplevel in
	let ipt_str = "\"Hello World\"" in
	let expected = "Hello World" in
	Alcotest.(check string) "string literal" expected (parse_ast ipt_str)

let test_str_lit2() = 
	(* testing ascii escapes *)
	let parse_ast = parse_ast false string_literal_toplevel in
	let ipt_str = "\"\\n\"" in
	let expected = "\n" in
	Alcotest.(check string) 
		"string literal with newline" expected (parse_ast ipt_str);

	let ipt_str = "\"\\r\"" in
	let expected = "\r" in
	Alcotest.(check string) 
		"string literal with carriage return" expected (parse_ast ipt_str);

	let ipt_str = "\"\\t\"" in
	let expected = "\t" in
	Alcotest.(check string) 
		"string literal with tab" expected (parse_ast ipt_str);

	let ipt_str = "\"\\\"\"" in
	let expected = "\"" in
	Alcotest.(check string) 
		"string literal with double quote" expected (parse_ast ipt_str);

	let ipt_str = "\"\\\\\"" in
	let expected = "\\" in
	Alcotest.(check string) 
		"string literal with backslash" expected (parse_ast ipt_str)

(* TODO: write test for \x *)

let test_str_lit3() = 
	(* using \ to change line *)
	let parse_ast = parse_ast false string_literal_toplevel in
	let ipt_str = "\"Hello \\\nWorld\"" in
	let expected = "Hello World" in
	Alcotest.(check string) 
		"string literal with newline" expected (parse_ast ipt_str)

let test_func_sig1() = 
	(* function signature without parameters*)
	let parse_ast = parse_ast false fn_sig_toplevel in
	let ipt_str = "fn foo() -> i32;" in
	let expected = (None, "foo", None, None, Some "i32", None) in
	Alcotest.(check fn_sig_testable) 
		"function signature with name and return type" expected (parse_ast ipt_str);

	let ipt_str = "fn foo();" in
	let expected = (None, "foo", None, None, None, None) in
	Alcotest.(check fn_sig_testable) 
		"function signature without return type" expected (parse_ast ipt_str)

let test_func_sig2() = 
	(* function signature with parameters *) 
	let parse_ast = parse_ast false fn_sig_toplevel in
	let ipt_str = "fn foo(x: i32, y: i32) -> i32;" in
	let param1_pattern = ("x", Type "i32") in
	let param2_pattern = ("y", Type "i32") in
	let fn_param1 = Fn_Param_Pattern (None, param1_pattern) in
	let fn_param2 = Fn_Param_Pattern (None, param2_pattern) in
	let fn_params_list = [fn_param1; fn_param2] in
	let fn_params = (None, fn_params_list) in
	let expected = 
		(None, "foo", None, Some fn_params, Some "i32", None) in
	Alcotest.(check fn_sig_testable) 
		"function signature with parameters" expected (parse_ast ipt_str);

	let ipt_str = "fn foo(x: i32, y: i32);" in
	(* TODO: making sure that trailing comma also work here *)
	let expected = (None, "foo", None, Some (None, 
																					 [Fn_Param_Pattern (None,("x", Type("i32"))); 
																						Fn_Param_Pattern (None,("y", Type("i32")))]
																					), None, None) in
	Alcotest.(check fn_sig_testable) 
		"function signature with parameters without return type" expected (parse_ast ipt_str)

let test_func_sig3() = 
	(* test signatures with self *)
	let parse_ast_ = parse_ast false fn_sig_toplevel in
	let ipt_str = "fn foo(&self) -> i32;" in
	let mut = () in
	let self_param_v = Short_Hand(None, Ref_Only (Some mut, None)) in
	let fn_params_v = (Some self_param_v, []) in
	let expected = (None, "foo", None, Some fn_params_v, Some "i32", None) in
	Alcotest.(check fn_sig_testable) 
		"function signature with self" expected (parse_ast_ ipt_str);

	(* don't use shorthand self, use typed self instead *)
	let ipt_str = "fn foo(mut self: i32) -> i32;" in
	let self_param_v = Typed_Self(None, (Some mut, "i32")) in
	let fn_params_v = (Some self_param_v, []) in
	let expected = (None, "foo", None, Some fn_params_v, Some "i32", None) in
	Alcotest.(check fn_sig_testable) 
		"don't use shorthand self, use typed self instead" expected (parse_ast_ ipt_str);

	(* shorthand self + regular params *)
	let ipt_str = "fn foo(&self, x: i32) -> i32;" in
	let self_param_v = Short_Hand(None, Ref_Only (Some mut, None)) in
	let param1_pattern = ("x", Type "i32") in
	let fn_param1 = Fn_Param_Pattern (None, param1_pattern) in
	let fn_params_list = [fn_param1] in
	let fn_params_v = (Some self_param_v, fn_params_list) in
	let expected = (None, "foo", None, Some fn_params_v, Some "i32", None) in
	Alcotest.(check fn_sig_testable) 
		"shorthand self + regular params" expected (parse_ast_ ipt_str);

	(* with lifetime param *)
	let ipt_str = "fn foo(&'a mut self) -> i32;" in
	(* TODO: add <'a> after implementing generic param*)
	let reference = () in
	let shorthand_self_v = Ref_Lifetime(Some (reference, "a"), Some mut) in 
	let self_param_v = Short_Hand(None, shorthand_self_v) in
	let fn_params_v = (Some self_param_v, []) in
	let expected = (None, "foo", None, Some fn_params_v, Some "i32", None) in
	Alcotest.(check fn_sig_testable)
		"function signature with self" expected (parse_ast_ ipt_str)

let test_func_sig4() = 
	(* test situations with function qualifiers *)
	let ipt_str = "extern \"c\" fn foo();" in
	let parse_ast_ = parse_ast false fn_sig_toplevel in
	let abi = "c" in
	let extern_qualifier = Extern (Some abi) in
	let expected = (Some extern_qualifier, "foo", None, None, None, None) in
	Alcotest.(check fn_sig_testable)
		"function signature with extern qualifier" expected (parse_ast_ ipt_str);

	let ipt_str = "const fn foo();" in
	let const_qualifier = Const in
	let expected = (Some const_qualifier, "foo", None, None, None, None) in
	Alcotest.(check fn_sig_testable)
		"function signature with const qualifier" expected (parse_ast_ ipt_str)

let test_int_literals1() = 
	(* test integer literals *)
	let parse_ast_ = parse_ast false integer_literal_toplevel in
	let expected = Dec_Int_Lit "123" in
	Alcotest.(check int_lit_testable)
		"dec integer literal" expected (parse_ast_ "123");

	let expected = Bin_Int_Lit "0b101" in
	Alcotest.(check int_lit_testable) 
		"bin integer literal" expected (parse_ast_ "0b101");

	let expected = Oct_Int_Lit "0o123_456" in
	Alcotest.(check int_lit_testable) 
		"oct integer literal" expected (parse_ast_ "0o123_456");

	let expected = Hex_Int_Lit "0xFFF_AAA_BB_CCC_u123" in
	Alcotest.(check int_lit_testable) 
		"hex integer literal" expected (parse_ast_ "0xFFF_AAA_BB_CCC_u123")

let test_float_literal1() = 
	(* test float literals *)
	let parse_ast_ = parse_ast false float_literal_toplevel in
	let lits = ["123.456"; "123."; "123e10"; "123.0f64"; 
							"12E+99_f64"; "0.1f32"; "114E-514"] in
	List.iter (fun x -> 
			Alcotest.(check string) 
				"float literal" x (parse_ast_ x)) lits;

	let lits_invalid = ["123.456.789"; "123e10.0"; "123.0f64.0"; 
											"12E+99_f64.0"; "0.1f32.0"; "114E-514.0"] in
	(* should not be equal *)
	List.iter (fun x -> 
			Alcotest.(check bool) 
				"invalid float literal" true (parse_ast_ x <> x)) lits_invalid

let test_op_expr1() = 

	(* test binary operator expresions *)
	let parse_ast = parse_ast false op_expr_toplevel in 
	let ipt_str = "1 + 2" in
	let fir_lit_expr = Literal_Expr (Integer_Literal (Dec_Int_Lit "1")) in
	let sec_lit_expr = Literal_Expr (Integer_Literal (Dec_Int_Lit "2")) in
	let fir_expr = Expr_Without_Block (None, fir_lit_expr) in
	let sec_expr = Expr_Without_Block (None, sec_lit_expr) in
	let expected = Binary (fir_expr, Add, sec_expr) in
	let actual = parse_ast ipt_str in
	Alcotest.(check op_expr_testable) "only addition" expected actual;

	let ipt_str = "1 + 2 * 3" in
	let fir_lit_expr = Literal_Expr (Integer_Literal (Dec_Int_Lit "1")) in
	let sec_lit_expr = Literal_Expr (Integer_Literal (Dec_Int_Lit "2")) in
	let thi_lit_expr = Literal_Expr (Integer_Literal (Dec_Int_Lit "3")) in
	let fir_expr = Expr_Without_Block (None, fir_lit_expr) in
	let sec_expr = Expr_Without_Block (None, sec_lit_expr) in
	let thi_expr = Expr_Without_Block (None, thi_lit_expr) in
	let mul_sub_expr = Expr_Without_Block(None, Op_Expr(Binary (sec_expr, Mul, thi_expr))) in 
	let expected = Binary (fir_expr, Add, mul_sub_expr) in
	let actual = parse_ast ipt_str in
	Alcotest.(check op_expr_testable) "addition and multiplication" expected actual; 

	let ipt_str = "(1 + 2) * 3" in
	let fir_lit_expr = Literal_Expr (Integer_Literal (Dec_Int_Lit "1")) in
	let sec_lit_expr = Literal_Expr (Integer_Literal (Dec_Int_Lit "2")) in
	let thr_lit_expr = Literal_Expr (Integer_Literal (Dec_Int_Lit "3")) in
	let fir_expr = Expr_Without_Block (None, fir_lit_expr) in
	let sec_expr = Expr_Without_Block (None, sec_lit_expr) in
	let thi_expr = Expr_Without_Block (None, thr_lit_expr) in
	let add_expr = Grouped_Expr(Expr_Without_Block
									(None, Op_Expr(Binary (fir_expr, Add, sec_expr)))) in
	let expected = Binary (Expr_Without_Block(None, add_expr), Mul, thi_expr) in
	let actual = parse_ast ipt_str in
	Alcotest.(check op_expr_testable) "with parentheses" expected actual;

	let ipt_str = "(1) / (2)" in 
	let fir_lit_expr = Literal_Expr (Integer_Literal (Dec_Int_Lit "1")) in
	let sec_lit_expr = Literal_Expr (Integer_Literal (Dec_Int_Lit "2")) in
	let fir_expr = Expr_Without_Block(None, 
									Grouped_Expr(Expr_Without_Block (None, fir_lit_expr))) in
	let sec_expr = Expr_Without_Block(None, 
									Grouped_Expr(Expr_Without_Block (None, sec_lit_expr))) in
	let expected = Binary (fir_expr, Div, sec_expr) in
	let actual = parse_ast ipt_str in
	Alcotest.(check op_expr_testable) "parantheses on single number" expected actual; 

	let ipt_str = "{1} < {2} " in 
	let fir_lit_expr = Literal_Expr (Integer_Literal (Dec_Int_Lit "1")) in
	let sec_lit_expr = Literal_Expr (Integer_Literal (Dec_Int_Lit "2")) in
	let fir_block = Block_Expr(None, [], Some fir_lit_expr) in
	let sec_block = Block_Expr(None, [], Some sec_lit_expr) in
	let fir_expr = Expr_With_Block(None, fir_block) in
	let sec_expr = Expr_With_Block(None, sec_block) in
	let expected = Binary (fir_expr, Lt, sec_expr) in
	let actual = parse_ast ipt_str in
	Alcotest.(check op_expr_testable) "block expressions" expected actual

let test_op_expr2() = 
	(* test unary operator expressions *)
	let parse_ast = parse_ast false op_expr_toplevel in
	let ipt_str = "-1" in
	let fir_lit_expr = Literal_Expr (Integer_Literal (Dec_Int_Lit "1")) in
	let fir_expr = Expr_Without_Block (None, fir_lit_expr) in
	let expected = Unary (Neg, fir_expr) in
	let actual = parse_ast ipt_str in
	Alcotest.(check op_expr_testable) "negation" expected actual; 

	let ipt_str = "-1 * 2 + 3" in 
	let fir_lit_expr = Literal_Expr (Integer_Literal (Dec_Int_Lit "1")) in
	let sec_lit_expr = Literal_Expr (Integer_Literal (Dec_Int_Lit "2")) in
	let thi_lit_expr = Literal_Expr (Integer_Literal (Dec_Int_Lit "3")) in
	let fir_expr = Expr_Without_Block (None, fir_lit_expr) in
	let sec_expr = Expr_Without_Block (None, sec_lit_expr) in
	let thi_expr = Expr_Without_Block (None, thi_lit_expr) in
	let neg_expr = Expr_Without_Block (None, Op_Expr(Unary (Neg, fir_expr))) in
	let neg_mul_expr = Expr_Without_Block (None, Op_Expr(Binary (neg_expr, Mul, sec_expr))) in
	let expected = Binary (neg_mul_expr, Add, thi_expr) in
	let actual = parse_ast ipt_str in
	Alcotest.(check op_expr_testable) "negation and multiplication" expected actual

let test_array_expr1() =  
	let parse_ast = parse_ast false array_expr_toplevel in
	let ipt_str = "[1, 2, 3]" in
	let fir_lit_expr = Literal_Expr (Integer_Literal (Dec_Int_Lit "1")) in
	let sec_lit_expr = Literal_Expr (Integer_Literal (Dec_Int_Lit "2")) in
	let thi_lit_expr = Literal_Expr (Integer_Literal (Dec_Int_Lit "3")) in
	let fir_expr = Expr_Without_Block (None, fir_lit_expr) in
	let sec_expr = Expr_Without_Block (None, sec_lit_expr) in
	let thi_expr = Expr_Without_Block (None, thi_lit_expr) in
	let expected = Exprs([fir_expr; sec_expr; thi_expr]) in
	let actual = parse_ast ipt_str in
	Alcotest.(check array_expr_testable) "array expression" expected actual; 

	let ipt_str = "[1; 2+3]" in 
	let fir_lit_expr = Literal_Expr (Integer_Literal (Dec_Int_Lit "1")) in
	let sec_lit_expr = Literal_Expr (Integer_Literal (Dec_Int_Lit "2")) in
	let thi_lit_expr = Literal_Expr (Integer_Literal (Dec_Int_Lit "3")) in
	let fir_expr = Expr_Without_Block (None, fir_lit_expr) in
	let sec_expr = Expr_Without_Block (None, sec_lit_expr) in
	let thi_expr = Expr_Without_Block (None, thi_lit_expr) in
	let add_expr = Expr_Without_Block (None, Op_Expr(Binary (sec_expr, Add, thi_expr))) in
	let expected = Repeat(fir_expr, add_expr) in
	let actual = parse_ast ipt_str in
	Alcotest.(check array_expr_testable) "array expression with semicolon" expected actual

let test_array_expr2() = 
	(* test array indexing *)
	let parse_ast = parse_ast false expr_toplevel in 
	let ipt_str = "arr[1]" in
	let arr_expr = Expr_Without_Block (None, Intermedaite (Place_Or_Struct_Unit "arr")) in
	let index_expr = Expr_Without_Block (None, Literal_Expr (Integer_Literal (Dec_Int_Lit "1"))) in
	let expected = Expr_Without_Block (None, Index_Expr(arr_expr, index_expr)) in
	let actual = parse_ast ipt_str in
	Alcotest.(check expr_testable) "array indexing" expected actual

let test_struct_expr1() = 
	let parser_ast = parse_ast false struct_expr_toplevel in
	let ipt_str = "Point {x: 10.0, y: 20.0}" in 
	let lit1 = Literal_Expr (Float_Literal "10.0") in
	let lit2 = Literal_Expr (Float_Literal "20.0") in
	let field1 = With_Expr_Ident(None, "x", Expr_Without_Block(None, lit1)) in
	let field2 = With_Expr_Ident(None, "y", Expr_Without_Block(None, lit2)) in
	let expected = Struct_Expr_Struct("Point", [field1; field2], None) in
	let actual = parser_ast ipt_str in
	Alcotest.(check struct_expr_testable) "struct struct" expected actual

(* let expr for the next *)

let eliminate_invisible_chars str = 
	let str = Str.global_replace (Str.regexp "[\n\t ]") "" str in
	let str = Str.global_replace (Str.regexp "\n") "" str in
	str

let str_testable = Alcotest.testable (fun fmt x -> Format.fprintf fmt "%s" x) 
				(fun x y -> eliminate_invisible_chars x = eliminate_invisible_chars y)

let test_preprocess_if() = 
	let ipt_str = "if(A{}).x(){}" in 
	let expected_str = "if((A{}).x()){}" in 
	let actual_str = preprocess_if_expr ipt_str in
	Alcotest.(check str_testable) "if expression" expected_str actual_str;

	let ipt_str = "if A{}{}" in 
	let expected_str = "if( A){}{}" in 
	let actual_str = preprocess_if_expr ipt_str in
	Alcotest.(check str_testable) "if expression without parentheses" expected_str actual_str;

	let ipt_str = "if A{}+B{}{}" in
	let expected_str = "if( A){}+B{}{}" in 
	let actual_str = preprocess_if_expr ipt_str in
	Alcotest.(check str_testable) "if expression with binary operator" expected_str actual_str;

	let ipt_str = "if a{}" in 
	let expected_str = "if( a){}" in
	let actual_str = preprocess_if_expr ipt_str in
	Alcotest.(check str_testable) "if expression with single letter" expected_str actual_str;
	
	print_endline (preprocess_if_expr "if if true {true} else {false} {}");

	let ipt_str = "a b c d e f struct something" in 
	let expected_str = "a b c d e f struct something" in
	let actual_str = preprocess_if_expr ipt_str in
	Alcotest.(check str_testable) "no if expression" expected_str actual_str
	(* test if let *)

let test_if_expr() = 
	let parse_ast = parse_ast true expr_toplevel in
	let ipt_str = "if 1 {1} else {2}" in
	let fir_lit_expr = Literal_Expr (Integer_Literal (Dec_Int_Lit "1")) in
	let sec_lit_expr = Literal_Expr (Integer_Literal (Dec_Int_Lit "2")) in
	let cond_expr  = Expr_Without_Block (None, fir_lit_expr) in
	let block_ret_expr = Block_Expr(None, [], Some sec_lit_expr) in
	let fir_block_exr = (None, [], Some fir_lit_expr) in
	let sec_block_expr = (None, [], Some sec_lit_expr) in 
	let else_block = Else_Block(sec_block_expr) in
	let expected = Expr_With_Block(None, If_Expr(cond_expr, fir_block_exr, Some else_block)) in
	let actual = parse_ast ipt_str in
	Alcotest.(check expr_testable) "if expression" expected actual

let () = let open Alcotest in run "unit tests" [
		"struct-case", [
			test_case "struct struct" `Quick test_struct_def1;
										test_case "unit structs" `Quick test_struct_def2;
										test_case "tuple structs" `Quick test_struct_def3;
									 ];
		"string-literal", [test_case "string literal" `Quick test_str_lit1;
											 test_case "string literal  newline" `Quick test_str_lit2;
											 test_case "string literal with newline" `Quick test_str_lit3;

											];
		"function-signature", [test_case "function signature" `Quick test_func_sig1; 
													 test_case "function signature with parameters" `Quick test_func_sig2;
													 test_case "function signature with self" `Quick test_func_sig3;
													 test_case "function signature with qualifiers" `Quick test_func_sig4
													];

		"integer-literals", [test_case "integer literals" `Quick test_int_literals1];
		"float-literals", [test_case "float literals" `Quick test_float_literal1];
		"operator-expressions", [test_case "operator expressions" `Quick test_op_expr1;
															test_case "unary operator expressions" `Quick test_op_expr2];

		"array-expressions", [test_case "array expressions" `Quick test_array_expr1;
													test_case "array indexing" `Quick test_array_expr2];
		"struct-expressions", [test_case "struct expressions" `Quick test_struct_expr1];
		"preprocess", [test_case "preprocess if" `Quick test_preprocess_if];
		"if-expr", [test_case "if expression" `Quick test_if_expr]

	]