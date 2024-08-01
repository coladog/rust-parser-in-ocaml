open Lexer
open Syntax 

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