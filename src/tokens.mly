%{

%}

// ref: https://doc.rust-lang.org/reference/lexical-structure.html
// 2.2 keywords
%token KW_AS
%token KW_BREAK
%token KW_CONST
%token KW_CONTINUE
%token KW_CRATE
%token KW_ELSE
%token KW_ENUM
%token KW_EXTERN
%token KW_FALSE
%token KW_FN
%token KW_FOR
%token KW_IF
%token KW_IMPL
%token KW_IN
%token KW_LET
%token KW_LOOP
%token KW_MATCH
%token KW_MOD
%token KW_MOVE
%token KW_MUT
%token KW_PUB
%token KW_REF
%token KW_RETURN
%token KW_SELFVALUE
%token KW_SELFTYPE
%token KW_STATIC
%token KW_STRUCT
%token KW_SUPER
%token KW_TRAIT
%token KW_TRUE
%token KW_TYPE
%token KW_UNSAFE
%token KW_USE
%token KW_WHERE
%token KW_WHILE

// since Rust 2018

%token KW_ASYNC
%token KW_AWAIT
%token KW_DYN

// rserved words

%token KW_ABSTRACT
%token KW_BECOME
%token KW_BOX
%token KW_DO
%token KW_FINAL
%token KW_MACRO
%token KW_OVERRIDE
%token KW_PRIV
%token KW_TYPEOF
%token KW_UNSIZED
%token KW_VIRTUAL
%token KW_YIELD

// since Rust 2018

%token KW_TRY

// weak keywords 

%token KW_MACRO_RULES 
%token KW_UNION
%token KW_STATICLIFETIME // 'static

// 2.3 Identifiers

%token <string> IDENT
// integer literals https://doc.rust-lang.org/reference/tokens.html#integer-literals
%token <string * string option> DEC_LITERAL BIN_LITERAL OCT_LITERAL HEX_LITERAL
// first string is the literal, second string is the suffix

// TODO: float literals


// 2.6 Tokens

// punctuations
%token PLUS MINUS STAR SLASH
%token PERCENT
%token CARET
%token NOT AND OR ANDAND OROR
%token SHL SHLEQ
%token SHR SHREQ
%token PLUSEQ MINUSEQ STAREQ SLASHEQ PERCENTEQ CARETEQ ANDEQ OREQ
%token EQ EQEQ NE
%token GT LT GE LE
%token AT
%token UNDERSCORE
%token DOT DOTDOT DOTDOTDOT DOTDOTEQ
%token COMMA
%token SEMI
%token COLON
%token PATHSEP
%token ARROW ARROWFAT LARROW
%token POUND
%token DOLLAR
%token QUESTION
%token TILDE
%token LPAREN RPAREN
%token LBRACKET RBRACKET
%token LBRACE RBRACE

%token QUOTE_ESC_SGL QUOTE_ESC_DBL // quote escape single and double 
%token ASCII_ESC_X ASCII_ESC_N ASCII_ESC_R ASCII_ESC_T ASCII_ESC_SLASH ASCII_ESC_0
%token UNICODE_ESC
%token <string> STR_LIT // string literal

%token LIFETIME_QUOTE

%token UMINUS UPLUS UNOT UREF UMUTREF

%token EOF

// lots of placeholders
%token IMPOSSIBLE_TO_MATCH1 IMPOSSIBLE_TO_MATCH2 IMPOSSIBLE_TO_MATCH3 IMPOSSIBLE_TO_MATCH4 IMPOSSIBLE_TO_MATCH5 IMPOSSIBLE_TO_MATCH6 IMPOSSIBLE_TO_MATCH7 IMPOSSIBLE_TO_MATCH8 IMPOSSIBLE_TO_MATCH9 IMPOSSIBLE_TO_MATCH10 

%%