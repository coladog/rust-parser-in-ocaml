open RSprinter
open RSsyntax

let () = 
  let expr = Expr_Without_Block (None, Literal_Expr (Integer_Literal (Dec_Int_Lit "42"))) in
  print_endline (print_expr expr)