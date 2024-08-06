open RSlexer
open RSsyntax 
open Str 

let rec create_directory_recursively path =
  let parent_dir = Filename.dirname path in (* exclude the last part *)
  if not (directory_exists parent_dir) then
    create_directory_recursively parent_dir;
  if not (directory_exists path) then
    Unix.mkdir path 0o777
and directory_exists path =
  try
    let stats = Unix.lstat path in
    stats.st_kind = S_DIR
  with Unix.Unix_error (ENOENT, _, _) -> false

let read_whole_file ic = 
	let rec read_whole_file' ic acc = 
		try
			let line = input_line ic in
			read_whole_file' ic (acc ^ line ^ "\n")
		with End_of_file -> acc in
	read_whole_file' ic ""

let preprocess_if_expr str = 
	(* store the string into a temp file, then call the python func in src *)
	create_directory_recursively "./_build/preprocess/";
	let build_folder_exists = Sys.file_exists "_build" in
	let t = Unix.time () in
	let local_t = Unix.localtime t in
	let filename = Printf.sprintf "./_build/preprocess/p_%d_%d_%d_%d;%d_%d" 
					(local_t.tm_year + 1900) (local_t.tm_mon + 1) local_t.tm_mday local_t.tm_hour 
					local_t.tm_min local_t.tm_sec in
	let timeofday = Unix.gettimeofday () in
	let unique_filename = filename ^ string_of_float timeofday in

	(* create the .in file *)
	let oc = open_out (unique_filename ^ ".in") in (* out channel *)
	let absolute_in_filename = Unix.realpath (unique_filename ^ ".in") in
	Printf.fprintf oc "%s" str;
	close_out oc;

	Sys.command ("../src/preprocessor.py " ^ "\"" ^ absolute_in_filename ^ "\"") |> ignore;
	(* create the .out file *)

	let ic = open_in (unique_filename ^ ".out") in
	let str = read_whole_file ic in
	close_in ic;
	str


let parse_ast use_pp root_element str =
	let sref = ref str in
	if use_pp then 
		sref := preprocess_if_expr str;
	let lexbuf = Lexing.from_string !sref in
	let ast = root_element RSlexer.read_token lexbuf in
	ast  

let item_testable = Alcotest.testable RSsyntax.pp_item RSsyntax.equal_item
let fn_sig_testable = Alcotest.testable RSsyntax.pp_fn_sig RSsyntax.equal_fn_sig
let int_lit_testable = Alcotest.testable RSsyntax.pp_integer_literal RSsyntax.equal_integer_literal
let op_expr_testable = Alcotest.testable RSsyntax.pp_op_expr RSsyntax.equal_op_expr
(* let literal_expr_testable = Alcotest.testable Syntax.pp_literal_expr Syntax.equal_literal_expr *)
let expr_testable = Alcotest.testable RSsyntax.pp_expr RSsyntax.equal_expr
let array_expr_testable = Alcotest.testable RSsyntax.pp_array_expr RSsyntax.equal_array_expr
let index_expr_testable = Alcotest.testable RSsyntax.pp_tuple_index_expr RSsyntax.equal_tuple_index_expr
let struct_expr_testable = Alcotest.testable RSsyntax.pp_struct_expr RSsyntax.equal_struct_expr