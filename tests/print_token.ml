open Syntax 
open Lexer

(* type tk = 
  | Tk of Tokens.token
[@@deriving show, eq]

let token_list_of_string str = 
  let lexbuf = Lexing.from_string str in 
  let rec loop acc = 
    match Lexer.read_token lexbuf with 
    | EOF -> List.rev acc
    | token -> loop (token::acc)
  in
  loop []

let () = 
  let str = input_line stdin in
  let tokens = token_list_of_string str in
  List.iter (fun token -> tk.pp (Tk token)) tokens *)

let token_list_of_string str = 
  let lexbuf = Lexing.from_string str in 
  let rec loop acc = 
    match Lexer.read_token lexbuf with 
    | EOF -> List.rev acc
    | _ -> loop ((Lexing.lexeme lexbuf)::acc)
  in 
  loop []

let () =
  let str = input_line stdin in
  let tokens = token_list_of_string str in
  (* print the length of tokens *)
  Printf.printf "tokens length %d\n" (List.length tokens);
  for i = 0 to List.length tokens - 1 do
    Printf.printf "read token: %s\n" (List.nth tokens i)
  done 