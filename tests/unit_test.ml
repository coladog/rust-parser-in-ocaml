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
  let expected  = (Vis_Item (None, (Some Pub),
                             (Struct_Vis_Item
                                (Struct
                                   ("Point", None, None,
                                    (Some [(None, None, "x", "i32"); (None, None, "y", "i32")]))))
                            )) in
  Alcotest.(check item_testable) "same item" expected (parse_ast ipt_str);

  let ipt_str = "
  pub struct Point {
    x: i32,
    y: i32
  }" in (* should be the same without the comma *)
  Alcotest.(check item_testable) "same item without comma for fields" expected (parse_ast ipt_str);

  let ipt_str = "
  pub struct Point {
    x: i32,
    y: i32
  };" in (* should be the same with the semicolon *)

  Alcotest.(check item_testable) "same item with semicolon" expected (parse_ast ipt_str);

  let ipt_str = "
  struct Point{
    x: i32  
    y: i32
  }
  " in (* should raise exception since forgot to add comma *)
  Alcotest.check_raises "missing comma" (Parser.Error) (fun () -> let _ = parse_ast ipt_str in ())

let test_struct_def2 () = 
  (* unit structs *)
  let parse_ast = parse_ast item in
  let ipt_str = "
  pub crate struct Point;
  " in 
  let expected = (Vis_Item (None, (Some Pub_Crate), 
                            (Struct_Vis_Item (Struct ("Point", None, None, None))))) in
  Alcotest.(check item_testable) "unit struct" expected (parse_ast ipt_str);  
  let ipt_str = "
  struct Point;
  " in
  let expected = (Vis_Item (None, None, 
                            (Struct_Vis_Item (Struct ("Point", None, None, None)))) ) in
  Alcotest.(check item_testable) "unit struct without pub" expected (parse_ast ipt_str) 

let test_struct_def3 () = 
  (* tuple struct *)
  let parse_ast = parse_ast item in
  let ipt_str = " 
  pub struct Point(i32, i32);
  " in 
  let expected = (Vis_Item (None, (Some Pub), 
                            (Struct_Vis_Item 
                               (Tuple ("Point", None, 
                                       (Some [(None, None, "i32"); (None, None, "i32")]), None)))))  in
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
  Alcotest.(check string) "string literal with newline" expected (parse_ast ipt_str);

  let ipt_str = "\"\\r\"" in
  let expected = "\r" in
  Alcotest.(check string) "string literal with carriage return" expected (parse_ast ipt_str);

  let ipt_str = "\"\\t\"" in
  let expected = "\t" in
  Alcotest.(check string) "string literal with tab" expected (parse_ast ipt_str);

  let ipt_str = "\"\\\"\"" in
  let expected = "\"" in
  Alcotest.(check string) "string literal with double quote" expected (parse_ast ipt_str);

  let ipt_str = "\"\\\\\"" in
  let expected = "\\" in
  Alcotest.(check string) "string literal with backslash" expected (parse_ast ipt_str)

(* TODO: write test for \x *)

let test_str_lit3() = 
  (* using \ to change line *)
  let parse_ast = parse_ast string_literal in
  let ipt_str = "\"Hello \\\nWorld\"" in
  let expected = "Hello World" in
  Alcotest.(check string) "string literal with newline" expected (parse_ast ipt_str)

let test_func_sig1() = 
  (* function signature without parameters*)
  let parse_ast = parse_ast fn_sig in
  let ipt_str = "fn foo() -> i32;" in
  let expected = (None, "foo", None, None, Some "i32", None) in
  Alcotest.(check fn_sig_testable) "function signature with name and return type" expected (parse_ast ipt_str);

  let ipt_str = "fn foo();" in
  let expected = (None, "foo", None, None, None, None) in
  Alcotest.(check fn_sig_testable) "function signature without return type" expected (parse_ast ipt_str)

let test_func_sig2() = 
  (* function signature with parameters *) 
  let parse_ast = parse_ast fn_sig in
  let ipt_str = "fn foo(x: i32, y: i32) -> i32;" in
  let expected = (None, "foo", None, Some (None, 
                [Fn_Param_Pattern (None,("x", Type("i32"))); Fn_Param_Pattern (None,("y", Type("i32")))]
                ), Some "i32", None) in
  Alcotest.(check fn_sig_testable) "function signature with parameters" expected (parse_ast ipt_str);
  
  let ipt_str = "fn foo(x: i32, y: i32);" in
  let expected = (None, "foo", None, Some (None, 
                [Fn_Param_Pattern (None,("x", Type("i32"))); Fn_Param_Pattern (None,("y", Type("i32")))]
                ), None, None) in
  Alcotest.(check fn_sig_testable) "function signature with parameters without return type" expected (parse_ast ipt_str)

(* let test_func_sig3() = 
  (* test signatures with self *)
  let parse_ast = parse_ast fn_sig in
  let ipt_str = "fn foo(&self) -> i32;" in
  let expected = (None, "foo", None, Some (Some (Ref_Only (Some mut, None)), 
                [Fn_Param_Pattern (None,("self", Type(""))) ]
                ), Some "i32", None) in
  Alcotest.(check fn_sig_testable) "function signature with self" expected (parse_ast ipt_str); *)

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
                          test_case "function signature with parameters" `Quick test_func_sig2
    ]

  ]