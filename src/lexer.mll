{
open Tokens
open Lexing
exception Syntax_Error of string
let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <-
        { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
        }

let hex_to_char_string hex_str =
    let int_value = int_of_string ("0x" ^ hex_str) in
    let char_value = Char.chr int_value in
    String.make 1 char_value

(*
Refs: 
FrontC 
https://mukulrathi.com/create-your-own-programming-language/parsing-ocamllex-menhir/ 
*)
}

let decdigit = ['0'-'9']
let octdigit = ['0'-'7']
let hexdigit = ['0'-'9' 'a'-'f' 'A'-'F']
let letter = ['a'- 'z' 'A'-'Z']
let ident = (letter|'_')(letter|decdigit|'_')*
let whitespace = [' ' '\t']+ (* TODO: modify whitespace refer to https://doc.rust-lang.org/reference/whitespace.html*)
let escape = '\\' _
let newline = '\r' | '\n' | "\r\n"
let nothing = ""

rule read_token = parse 
    (* Key words *)
    | "as" { KW_AS }
    | "break" { KW_BREAK }
    | "const" { KW_CONST }
    | "continue" { KW_CONTINUE }
    | "crate" { KW_CRATE }
    | "else" { KW_ELSE }
    | "enum" { KW_ENUM }
    | "extern" { KW_EXTERN }
    | "false" { KW_FALSE }
    | "fn" { KW_FN }
    | "for" { KW_FOR }
    | "if" { KW_IF }
    | "impl" { KW_IMPL }
    | "in" { KW_IN }
    | "let" { KW_LET }
    | "loop" { KW_LOOP }
    | "match" { KW_MATCH }
    | "mod" { KW_MOD }
    | "move" { KW_MOVE }
    | "mut" { KW_MUT }
    | "pub" { KW_PUB }
    | "ref" { KW_REF }
    | "return" { KW_RETURN }
    | "self" { KW_SELFVALUE }
    | "Self" { KW_SELFTYPE }
    | "static" { KW_STATIC }
    | "struct" { KW_STRUCT }
    | "super" { KW_SUPER }
    | "trait" { KW_TRAIT }
    | "true" { KW_TRUE }
    | "type" { KW_TYPE }
    | "unsafe" { KW_UNSAFE }
    | "use" { KW_USE }
    | "where" { KW_WHERE }
    | "while" { KW_WHILE }
    | "async" { KW_ASYNC }
    | "await" { KW_AWAIT }
    | "dyn" { KW_DYN }
    | "abstract" { KW_ABSTRACT }
    | "become" { KW_BECOME }
    | "box" { KW_BOX }
    | "do" { KW_DO }
    | "final" { KW_FINAL }
    | "macro" { KW_MACRO }
    | "override" { KW_OVERRIDE }
    | "priv" { KW_PRIV }
    | "typeof" { KW_TYPEOF }
    | "unsized" { KW_UNSIZED }
    | "virtual" { KW_VIRTUAL }
    | "yield" { KW_YIELD }
    | "try" { KW_TRY }
    | "macro_rules" { KW_MACRO_RULES }
    | "union" { KW_UNION }
    | "'static" { KW_STATICLIFETIME }

    (* Punctuations *)

    | "+" { PLUS }
    | "-" { MINUS }
    | "*" { STAR }
    | "/" { SLASH }
    | "%" { PERCENT }
    | "^" { CARET }
    | "!" { NOT }
    | "&&" { ANDAND }
    | "||" { OROR }
    | "<<" { SHL }
    | "<<=" { SHLEQ }
    | ">>" { SHR }
    | ">>=" { SHREQ }
    | "+=" { PLUSEQ }
    | "-=" { MINUSEQ }
    | "*=" { STAREQ }
    | "/=" { SLASHEQ }
    | "%=" { PERCENTEQ }
    | "^=" { CARETEQ }
    | "&=" { ANDEQ }
    | "|=" { OREQ }
    | "==" { EQEQ }
    | "=" { EQ }
    | "!=" { NE }
    | ">" { GT }
    | "<" { LT }
    | ">=" { GE }
    | "<=" { LE }
    | "@" { AT }
    | "_" { UNDERSCORE }
    | "." { DOT }
    | ".." { DOTDOT }
    | "..." { DOTDOTDOT }
    | "..=" { DOTDOTEQ }
    | "," { COMMA }
    | ";" { SEMI }
    | ":" { COLON }
    | "::" { PATHSEP }
    | "->" { ARROW }
    | "=>" { ARROWFAT }
    | "<-" { LARROW }
    | "#" { POUND }
    | "$" { DOLLAR }
    | "?" { QUESTION }
    | "~" { TILDE }
    | "(" { LPAREN }
    | ")" { RPAREN }
    | "[" { LBRACKET }
    | "]" { RBRACKET }
    | "{" { LBRACE }
    | "}" { RBRACE }
    
    (* Others *)

    | "'" {LIFETIME_QUOTE}

    (* Literals related*)

    | ident { IDENT (Lexing.lexeme lexbuf) }
    | "\\\'"{QUOTE_ESC_SGL}
    | "\\\"" {QUOTE_ESC_DBL}
    | "\\x" {ASCII_ESC_X}
    | "\\n" {ASCII_ESC_N}
    | "\\r" {ASCII_ESC_R}
    | "\\t" {ASCII_ESC_T}
    | "\\\\" {ASCII_ESC_SLASH}
    | "\\0" {ASCII_ESC_0}
    | "\\u" {UNICODE_ESC}
    | '"' {read_string_literals (Buffer.create 15) lexbuf}

    (* Special chars *)
    | nothing {IMPOSSIBLE_TO_MATCH}
    | whitespace { next_line lexbuf; read_token lexbuf }
    | newline { next_line lexbuf; read_token lexbuf }
    | eof { EOF }
    | _ {raise (Syntax_Error ("Lexer: Invalid token " ^ Lexing.lexeme lexbuf))}

and read_single_line_comment = parse
    | newline {next_line lexbuf; read_token lexbuf}
    | eof {EOF}
    | _ {read_single_line_comment lexbuf}

and read_multi_line_comment = parse 
    | "*/" {read_token lexbuf}
    | newline {next_line lexbuf; read_multi_line_comment lexbuf}
    | eof {raise (Syntax_Error ("Lexer: Unterminated multi-line comment"))}
    | _ {read_multi_line_comment lexbuf}

and read_string_literals buf = parse
    | '"' {STR_LIT (Buffer.contents buf)} (* string literal ends here *)
    | "\\\n" {(* directly skip *) read_string_literals buf lexbuf}
    | "\\\'" {Buffer.add_char buf '\''; read_string_literals buf lexbuf}
    | "\\\"" {Buffer.add_char buf '"'; read_string_literals buf lexbuf}
    | "\\x" {Buffer.add_string buf "\\x"; read_string_literals buf lexbuf}
    | "\\n" {Buffer.add_char buf '\n'; read_string_literals buf lexbuf}
    | "\\r" {Buffer.add_char buf '\r'; read_string_literals buf lexbuf}
    | "\\t" {Buffer.add_char buf '\t'; read_string_literals buf lexbuf}
    | "\\\\" {Buffer.add_char buf '\\'; read_string_literals buf lexbuf}
    | "\\0" {Buffer.add_char buf '\000'; read_string_literals buf lexbuf}
    | [^ '\\' '"']+ {Buffer.add_string buf (Lexing.lexeme lexbuf); read_string_literals buf lexbuf}
    | eof {raise (Syntax_Error ("Lexer: Unterminated string literal"))}
    | _ {raise (Syntax_Error ("Lexer: Invalid string literal" ^ Lexing.lexeme lexbuf))}
    (* | "\\u" {read_escape_u buf lexbuf}  TODO: implement UNICODE*) 
and read_escape_x buf = parse
    | hexdigit as hex {Buffer.add_char buf hex; read_escape_x buf lexbuf}
    | _ {raise (Syntax_Error ("Lexer: Invalid escape sequence, \\x have to be followed by hexidecimal digits"))}