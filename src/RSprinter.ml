open RSsyntax 
let rec print_expr (exp : expr) = 
  match exp with 
    | Expr_Without_Block (oa, e) -> 
      print_outer_attr oa ^
      print_expr_without_block e
    | Expr_With_Block (oa, e) -> 
      print_outer_attr oa ^
      print_expr_with_block e 

and print_expr_without_block (ewb : expr_without_block) = 
  match ewb with 
    | Literal_Expr le -> ""
    | Op_Expr oe -> print_op_expr oe
    | Grouped_Expr ge -> grouped_expr ge
    | _ -> ""

and print_expr_with_block (ewb : expr_with_block) = 
  match ewb with 
    | _ -> ""

and print_literal_expr (le : literal_expr) = 
  match le with
    | String_Literal s -> s
    | Byte_Literal b -> b 
    | Integer_Literal i -> print_integer_literal i 
    | Float_Literal f -> f

and print_integer_literal (il : integer_literal) = 
  match il with 
    | Dec_Int_Lit d -> d 
    | Bin_Int_Lit b -> b
    | Oct_Int_Lit o -> o
    | Hex_Int_Lit h -> h

and print_op_expr (oe : op_expr) = 
  match oe with 
    | _ -> ""

and grouped_expr (ge : expr) =
  "(" ^ print_expr ge ^ ")"

and print_outer_attr (oa : outer_attrs option) = 
  ""