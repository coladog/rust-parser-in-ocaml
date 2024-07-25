open Utils
open Syntax 
open Parser

let test_struct_def1 () = 
	(* with visibility params and fields *)
	let parse_ast = parse_ast item in
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
	"missing comma" (Parser.Error) (fun () -> let _ = parse_ast ipt_str in ())

let test_struct_def2 () = 
	(* unit structs *)
	let parse_ast = parse_ast item in
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
	let parse_ast = parse_ast item in
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
	let parse_ast = parse_ast string_literal in
	let ipt_str = "\"Hello World\"" in
	let expected = "Hello World" in
	Alcotest.(check string) "string literal" expected (parse_ast ipt_str)

let test_str_lit2() = 
	(* testing ascii escapes *)
	let parse_ast = parse_ast string_literal in
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
	let parse_ast = parse_ast string_literal in
	let ipt_str = "\"Hello \\\nWorld\"" in
	let expected = "Hello World" in
	Alcotest.(check string) 
	"string literal with newline" expected (parse_ast ipt_str)

let test_func_sig1() = 
	(* function signature without parameters*)
	let parse_ast = parse_ast fn_sig in
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
	let parse_ast = parse_ast fn_sig in
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
	let parse_ast_ = parse_ast fn_sig in
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
	let parse_ast_ = parse_ast fn_sig in
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
	let parse_ast_ = parse_ast integer_literal in
	let expected = Dec_Int_Lit "123" in
	Alcotest.(check int_lit_testable) "dec integer literal" expected (parse_ast_ "123");

	let expected = Bin_Int_Lit "0b101" in
	Alcotest.(check int_lit_testable) "bin integer literal" expected (parse_ast_ "0b101");
	
	let expected = Oct_Int_Lit "0o123_456" in
	Alcotest.(check int_lit_testable) "oct integer literal" expected (parse_ast_ "0o123_456");
	
	let expected = Hex_Int_Lit "0xFFF_AAA_BB_CCC_u123" in
	Alcotest.(check int_lit_testable) "hex integer literal" expected (parse_ast_ "0xFFF_AAA_BB_CCC_u123")

let test_float_literal1() = 
	(* test float literals *)
	let parse_ast_ = parse_ast float_literal in
	let lits = ["123.456"; "123."; "123e10"; "123.0f64"; "12E+99_f64"; "0.1f32"; "114E-514"] in
	List.iter (fun x -> Alcotest.(check string) "float literal" x (parse_ast_ x)) lits;

	let lits_invalid = ["123.456.789"; "123e10.0"; "123.0f64.0"; "12E+99_f64.0"; "0.1f32.0"; "114E-514.0"] in
	(* should not be equal *)
	List.iter (fun x -> Alcotest.(check bool) "invalid float literal" true (parse_ast_ x <> x)) lits_invalid
	

let () = let open Alcotest in run "unit tests" [
		"struct-case", [test_case "struct struct" `Quick test_struct_def1;
										test_case "unit structs" `Quick test_struct_def2;
										test_case "tuple structs" `Quick test_struct_def3;
									 ];
		"string-literal", [test_case "string literal" `Quick test_str_lit1;
											test_case "string literal with newline" `Quick test_str_lit2;
											test_case "string literal with newline" `Quick test_str_lit3;
		
		];
		"function-signature", [test_case "function signature" `Quick test_func_sig1; 
													test_case "function signature with parameters" `Quick test_func_sig2;
													test_case "function signature with self" `Quick test_func_sig3;
													test_case "function signature with qualifiers" `Quick test_func_sig4
		];

		"integer-literals", [test_case "integer literals" `Quick test_int_literals1];
		"float-literals", [test_case "float literals" `Quick test_float_literal1]

	]