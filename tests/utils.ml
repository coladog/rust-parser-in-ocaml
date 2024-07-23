open Lexer
open Syntax 

let parse_ast root_element str = 
  let lexbuf = Lexing.from_string str in
  let ast = root_element Lexer.read_token lexbuf in
ast

let item_testable = Alcotest.testable Syntax.pp_item Syntax.equal_item