open Utils
open Syntax 
open Parser
let test_struct_def1 () = 
  (* with visibility params and fields *)
  let ipt_str = "
  pub struct Point {
    x: i32,
    y: i32,
  }" in
  let expected  = (Vis_Item ([], (Some Pub),
                             (Struct_vis_item
                                (Struct
                                   ("Point", None, None,
                                    (Some [([], None, "x", "i32"); ([], None, "y", "i32")]))))
                            )) in
  Alcotest.(check item_testable) "same item" expected (parse_ast ipt_str);
  
  let ipt_str = "
  pub struct Point {
    x: i32,
    y: i32
  }" in (* should be the same without the comma *)
  Alcotest.(check item_testable) "same item" expected (parse_ast ipt_str);

  let ipt_str = "
  struct Point{
    x: i32  
    y: i32
  }
  " in (* should raise exception since forgot to add comma *)
  Alcotest.check_raises "missing comma" (Parser.Error) (fun () -> let _ = parse_ast ipt_str in ())

let () = let open Alcotest in run "unit tests" [
    "struct-case", [test_case "struct_def1" `Quick test_struct_def1]
  ]