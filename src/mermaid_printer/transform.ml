open Syntax 
type node_shape = 
  | RECT 
  | ROUND_EDGE
  | STADIUM
  | SUBROUTINE
  | CYLINDRICAL 
  | CIRCLE
  | ASYMMETRIC 
  | RHOMBUS
  | HEXAGON
  | PARALLELOGRAM
  | PARALLELOGRAM_ALT
  | TREPEZOID
  | TREPEZOID_ALT
  | DOUBLE_CIRCLE
[@@deriving show]
let style_str_of_shape= function
  | RECT -> ("[", "]")
  | ROUND_EDGE -> ("(", ")")
  | STADIUM -> ("([", "])")
  | SUBROUTINE -> ("[[", "]]")
  | CYLINDRICAL -> ("[(", ")]")
  | CIRCLE -> ("((", "))")
  | ASYMMETRIC -> (">", "]")
  | RHOMBUS -> ("{", "}")
  | HEXAGON -> ("{{", "}}")
  | PARALLELOGRAM -> ("/", "/")
  | PARALLELOGRAM_ALT -> ("\\", "\\")
  | TREPEZOID -> ("/", "\\")
  | TREPEZOID_ALT -> ("\\", "/")
  | DOUBLE_CIRCLE -> ("((", "))")
[@@deriving show]

let sytle_str_of_node = function 
  | _ -> style_str_of_shape RECT

let nid = ref 0

let get_inc() = 
  let cur_id = !nid in
  nid := !nid + 1;
  cur_id

let rec mermaid_str_of_node (n:node) : (string * int) =
  let Node(id_txt, ns) = n in
  let (sl, sr) = sytle_str_of_node n in
  let cur_id = get_inc() in
  let ret = ref ("node_id" ^ (string_of_int cur_id )^ sl ^ "\"" ^ id_txt ^ "\"" ^ sr ^ "\n") in 
  for i = 0 to List.length ns - 1 do
    let (child_s, child_id) = mermaid_str_of_node (List.nth ns i) in 
    ret := !ret ^ child_s;
    ret := !ret ^ ("node_id" ^ (string_of_int cur_id) ^ " --> " ^ "node_id" ^ (string_of_int child_id) ^ "\n")
  done;
  (!ret , cur_id)