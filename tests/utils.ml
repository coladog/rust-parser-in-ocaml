open Syntax 

let parse_ast s  = 
  let lexbuf = Lexing.from_string s in
  let ast = Parser.item Lexer.read_token lexbuf in
ast

let item_testable = Alcotest.testable Syntax.pp_item Syntax.equal_item