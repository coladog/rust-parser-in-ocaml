open Utils

let main () = 
  let struct_def_str = "
  pub struct Point {
      x: i32,
      y: i32,
  }
  "  in 
  (* print_endline struct_def_str;; *)
  print_endline (Syntax.show_item (parse_ast struct_def_str));;

main()