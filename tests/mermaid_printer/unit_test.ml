open Syntax
open Transform
let parse_ast str = 
  let lexbuf = Lexing.from_string str in
  Parser.node_toplevel Lexer.read_token lexbuf

let test_str = "(RSsyntax.Vis_Item (None, (Some RSsyntax.Pub),
                    (RSsyntax.Struct_Vis_Item
                       (RSsyntax.Tuple
                          (\"Point\", None,
                           (Some [(None, None, \"i32\"); (None, None, \"i32\")]),
                           None)))
                    ))"

let test_str = "(RSsyntax.Expr_With_Block (None,
                 (RSsyntax.If_Expr
                    ((RSsyntax.Expr_Without_Block (None,
                        (RSsyntax.Literal_Expr
                           (RSsyntax.Integer_Literal
                              (RSsyntax.Dec_Int_Lit 1)))
                        )),
                     (None, [],
                      (Some (RSsyntax.Literal_Expr
                               (RSsyntax.Integer_Literal
                                  (RSsyntax.Dec_Int_Lit 1))))),
                     (Some (RSsyntax.Else_Block
                              (None, [],
                               (Some (RSsyntax.Literal_Expr
                                        (RSsyntax.Integer_Literal
                                           (RSsyntax.Dec_Int_Lit 2)))))))))
                 ))"

(* let test_str = "BinOp(
BinOp(Int 1, Add, Int 2), Add, Int [1;2;3])" *)

(* let test_str = "A(B D, C)" *)

(* remove all /t/n for test_str *)
let test_str = String.trim (String.map (fun c -> if c = '\t' || c = '\n' then ' ' else c) test_str)

let () =
  let ast = parse_ast test_str in
  print_endline (show_node ast);
  let (m_str, _) = mermaid_str_of_node ast in
  print_endline m_str