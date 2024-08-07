{  
    open Syntax 
    open Parser
    exception Syntax_Error of string
}

let whitespace = [' ' '\t' '\r' '\n'] (* \s *)
let non_whitespace = [^ ' ' '\t''\r''\n'] (* \S *)
let ident = [^ ';' '[' ']' '(' ')' ' ' '\t' '\r' '\n' ',']+
let string_lit = ([^'"' '\\'] |'\\'(['x']|[^'x']))*

rule read_token = parse
    (* | string_lit {print_endline ("string lit of " ^ Lexing.lexeme lexbuf);
                IDENT (Lexing.lexeme lexbuf) } *)
    | '[' {LBRACK}
    | ']' {RBRACK}
    | ';' {SEMI}
    | ident {IDENT (Lexing.lexeme lexbuf)}
    | '(' {LPAREN}
    | ')' {RPAREN}
    | ',' {COMMA}
    | eof {EOF}
    | whitespace {read_token lexbuf}
    | _ {raise (Syntax_Error ("unexpected character of " ^ Lexing.lexeme lexbuf))}