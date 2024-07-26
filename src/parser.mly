%{
	open Syntax 
	open Tokens
	exception Parser_Error of string
%}

// precedence and associativity
// ref: https://doc.rust-lang.org/reference/expressions.html
%nonassoc OPTION_NONE LOWEST_PRIORITY

%nonassoc UMINUS UPLUS UNOT UREF UMUTREF

%right EQ PLUSEQ MINUSEQ MULEQ DIVEQ PERCENTEQ ANDEQ OREQ CARETEQ SHLEQ SHREQ
// require parentheses for .. and ..=
%left OROR
%left ANDAND
// require parentheses for ==, !=, <, >, <=, >=
%left OR
%left CARET
%left AND
%left SHL SHR
%nonassoc QUESTION


%nonassoc THEN // mark the if branch with this so that it have lower precedence than else
%nonassoc KW_ELSE

%nonassoc LPAREN LBRACE LBRACKET
%nonassoc RPAREN RBRACE RBRACKET

%right SEMI COMMA 

%nonassoc OPTION_SOME HIGHEST_PRIORITY 

// remove for productions (the start symbols)
%start <item> item
%start <string_literal> string_literal
%start <integer_literal> integer_literal
%start <float_literal> float_literal
%start <fn_sig> fn_sig
%start <self_param> self_param 
// %start <op_expr> op_expr
// %start <stmt> stmt
%start <struct_expr_struct> struct_expr_struct
%%

// separated_nonempty_list(sep, X):
// 	| l = separated_nonempty_list(sep, X) { l }

/* 2. Lexical structure */
	/* 2.6 Tokens */
string_literal: 
	| s = STR_LIT { s }
	
integer_literal: 
	| d = DEC_INT_LIT { Dec_Int_Lit d }
	| b = BIN_INT_LIT { Bin_Int_Lit b }
	| o = OCT_INT_LIT { Oct_Int_Lit o }
	| h = HEX_INT_LIT { Hex_Int_Lit h }

float_literal: 
	| f = FLOAT_LIT { f }

/* 6. Items */
item:
//TODO: ignoring all attributes at this moment, should fix it
	| oa = option(outer_attrs) v = option(visibility) vi = vis_item 
	  { Vis_Item(None, v, vi) }

vis_item: 
	| s = struct__ { Struct_Vis_Item(s) }


	/* 6.4 Functions*/

//  TODO: impl function with body
fn_sig:
	| fq = option(fn_qualifiers) KW_FN id = IDENT gp = option(generic_params) 
	  LPAREN fps = ioption(fn_params) RPAREN ret = option(fn_return_type) 
	  wc = option(where_clause) SEMI 
	  { (fq, id, None, fps, ret, None) }

fn_qualifiers: 
	| KW_CONST { Const }
	| KW_ASYNC { Async }
	| KW_UNSAFE { Unsafe }
	| KW_EXTERN abi = option(fn_abi) { Extern(abi) }

fn_abi: 
	| s = STR_LIT { s }

fn_params:
	| sp = self_param ioption(COMMA) { (Some sp, []) }
	| sp = ioption(terminated(self_param, COMMA)) 
		fps = separated_nonempty_list(COMMA, fn_param) { (sp, fps) }
// TODO: figure out if it is allowed for list to have a trailing comma
// have to use ioption to make it pass compile

self_param:
	| oa = option(outer_attrs) ss = shorthand_self 
	  { Short_Hand(None, ss) }
	| oa = option(outer_attrs) ts = typed_self 
	  { Typed_Self(None, ts) }

shorthand_self:
	| r = option(ref_lifetime) m = option(KW_MUT) KW_SELFVALUE { 
		match r with
		| None -> Ref_Only(None, m)
		| Some (r, Some l) -> Ref_Lifetime(Some (r, l), m)
		| Some (r, None) -> Ref_Only(Some r, m)
	 }
ref_lifetime:
	| r = AND { print_endline "matched AND"; (r, None)}
	| r = AND l = lifetime { print_endline "matched AND lifetime"; (r, Some l) }

typed_self:
	| KW_SELFVALUE__COLON t = type__  { (None, t) }
	| KW_MUT__KW_SELFVALUE__COLON t = type__ { (Some (), t) }

// have to use ioption to make it work, refer to https://stackoverflow.com/questions/26182521/seemingly-equivalent-menhir-rules-change-the-shift-reduce-conflicts-found-in-gra#comment138906002_26182521
fn_param: 
	| oa = option(outer_attrs) p = fn_param_pattern 
	  { Fn_Param_Pattern(None, p) }
	| oa = option(outer_attrs) t = type_or_elipsis 
	  { Type_Or_Elipsis(None, t) }

fn_param_pattern: 
	| p = pattern_no_top_alt COLON te = type_or_elipsis { (p, te) }

type_or_elipsis:
	| t = type__ { Type(t) }
	| DOTDOTDOT { Elipsis }

fn_return_type: 
	| ARROW t = type__ { t }

	/* 6.6 Structs */
struct__:
	| ss = struct_struct { Struct(ss) }
	| ts = tuple_struct { Tuple(ts) }

struct_struct: 
// TODO: add generic_params, where_clause
// struct_struct = string * generic_params option * where_clause option * struct_fields option
	| KW_STRUCT id = IDENT gp = option(generic_params) wc = option(where_clause)
	  LBRACE sf = option(struct_fields) RBRACE 
	  { (id, None, None, sf) } 
	| KW_STRUCT id = IDENT gp = option(generic_params) wc = option(where_clause) SEMI 
	  { (id, None, None, None) } // unit struct 
tuple_struct: 
// TODO: add generic_params, where_clause
// tuple_struct = string * generic_params option * tuple_fields option * where_clause option
	| KW_STRUCT id = IDENT gp = option(generic_params) LPAREN
	  tf = option(tuple_fields) RPAREN wc = option(where_clause) SEMI 
	  { (id, None, tf, None) }

struct_fields:
	| sf = struct_field option(COMMA) {[sf]}
	| sf = struct_field COMMA sfs = struct_fields {sf :: sfs}

struct_field: 
// TODO: add outer_attrs and actual type 
	| oa = option(outer_attrs) v = option(visibility) id = IDENT COLON tid = type__ 
	  { (None, v, id, tid) }

tuple_fields:
	| tf = tuple_field option(COMMA) {[tf]}
	| tf = tuple_field COMMA tfs = tuple_fields {tf :: tfs}

tuple_field: 
// TODO: add outer_attrs and actual type 
	| oa = option(outer_attrs) v = option(visibility) tid = type__ 
	  { (None, v, tid) }

	/* 8. Statement and expressions */
		/* 8.1 Statements */

stmt: 
	| SEMI {Semi}
	| i = item {Item i}
	| l = let_stmt {Let_Stmt l}
	| e = expr_stmt {Expr_Stmt e}
	// TODO: macro invocation stmt


let_stmt: 
	| oa = option(outer_attrs) KW_LET p = pattern_no_top_alt 
		t = option(preceded(COLON, type__)) 
		a = option(let_stmt_assignment) SEMI
		{
			(None, p, t, a)
		} 

let_stmt_assignment:
	| EQ e = expr e2 = option(preceded(KW_ELSE, block_expr)) { 
		(* When an else block is specified, 
			 the Expression must not be a LazyBooleanExpression, or end with a } *)
		if e2 = None then 
			(e, None)
		else 
			match e with
				| Expr_Without_Block(_, (Op_Expr (Binary (_, opor, _)))) -> (
					match opor with 
						| Lazy_And | Lazy_Or -> 
							raise (Parser_Error "Lazy boolean operator 
																		not allowed in let statement")
						| _ -> (e, e2)
					) 
				| Expr_With_Block(_, _) -> 
					raise (Parser_Error "Expression ending with } 
								not allowed in let statement")
				| _ -> (e, e2)
	}


// [1,2,3] [1,2,3,]
// (1,2,3,)
expr_stmt: 
	| e = expr_without_block SEMI {Expr_Without_Block_Stmt e}
	| e = expr_with_block SEMI {Expr_With_Block_Stmt e}
	| e = expr_with_block {Expr_With_Block_Stmt e} %prec OPTION_NONE

		/* 8.2 Expressions */
expr: 
	| oa = outer_attrs e = expr_without_block { Expr_Without_Block (None, e) }
	| oa = outer_attrs b = expr_with_block { Expr_With_Block (None, b) }

expr_without_block: 
	| l = literal_expr { Literal_Expr l }
	| o = op_expr { Op_Expr o }
	| a = array_expr { Array_Expr a }
	| t = tuple_expr { Tuple_Expr t }
	| t = tuple_index_expr { Tuple_Index_Expr t }
	| s = struct_expr { Struct_Expr s }
	| c = call_expr { Call_Expr c }
	| f = field_access_expr { Field_Access_Expr f }

expr_with_block:
	| b = block_expr { Block_Expr b }
	| i = if_expr { If_Expr i }
	| i = if_let_expr { If_Let_Expr i }
	| u = unsafe_block_expr { Unsafe_Block_Expr u }

			/* 8.2.1 Literal expressions */

literal_expr: 
	| s = string_literal { String_Literal s }
	// TODO | b = byte_literal { Byte_Literal b }
	| i = integer_literal { Integer_Literal i }
	| f = float_literal { Float_Literal f }

			/* 8.2.3 Block expressions */

block_expr: 
	| LBRACE ia = inner_attrs s = list(stmt) e = option(expr_without_block) RBRACE 
	  { (None, s, e) } 
	
async_block_expr: 
	| KW_ASYNC m = option(KW_MOVE) b = block_expr { (m, b) }

const_block_expr:
	| KW_CONST b = block_expr { b }

unsafe_block_expr:
	| KW_UNSAFE b = block_expr { b }

			/* 8.2.4 Operator expressions */

bin_opor:
	| PLUS { Add }      | MINUS { Sub } | STAR { Mul } | SLASH { Div } | PERCENT { Rem }
	| CARET { Bit_Xor } | AND { Bit_And }
	| OR { Bit_Or }     | SHL { Shl }   | SHR { Shr }
	| EQEQ { Eq }       | NE { Ne }     | LT { Lt }    | LE { Le }
	| GT { Gt }         | GE { Ge }
	| ANDAND { Lazy_And }    | OROR { Lazy_Or }
	| PLUSEQ { Add_Assign }  | MINUSEQ { Sub_Assign } | STAREQ { Mul_Assign }
	| SLASHEQ { Div_Assign } | PERCENTEQ { Rem_Assign }
	| ANDEQ { And_Assign }   | OREQ { Or_Assign }     | CARETEQ { Xor_Assign }
	| SHLEQ { Shl_Assign }   | SHREQ { Shr_Assign }

un_opor:
	| MINUS { Neg }
	| NOT { Not }
	| STAR { Deref }
	| AND { Ref }
	| AND KW_MUT { Mut_Ref }
	| QUESTION { Error_Propagation }

op_expr:
	| u = un_opor e = expr { Unary (u, e)}
	| e1 = expr b = bin_opor e2 = expr { Binary (e1, b, e2) }
	| e = expr KW_AS t = type_no_bounds { Cast (e, t) }

			/* 8.2.5 Grouped expressions */

group_expr: 
	| LPAREN e = expr RPAREN { e }

			/* 8.2.6 Array and index expressions */

array_expr: 
	| LBRACKET ls = separated_list_option_trailing(COMMA, expr) RBRACKET 
	  { Exprs ls }
	| LBRACKET e = expr SEMI rep = expr RBRACKET { Repeat (e, rep) }

index_expr: 
	| e = expr LBRACKET i = expr RBRACKET { (e, i) }

			/* 8.2.7 Tuple expressions */

tuple_expr: 
	| LPAREN ls = separated_list_option_trailing(COMMA, expr) RPAREN {ls}

tuple_index: 
	| i = integer_literal { i }

tuple_index_expr: 
	| e = expr DOT i = tuple_index { (e, i) }

			/* 8.2.8 Struct expressions */

struct_expr: 
	| s = struct_expr_struct { Struct_Expr_Struct s }
	| t = struct_expr_tuple { Struct_Expr_Tuple t }
	| u = struct_expr_unit { Struct_Expr_Unit u }

struct_expr_struct:
	| p = path_in_expr LBRACE fs = separated_list(COMMA, struct_expr_field) 
		ioption(COMMA) RBRACE 
		{ (p, fs, None) }
	// | p = path_in_expr LBRACE fs = separated_list(COMMA, struct_expr_field) 
	// 	COMMA b = struct_base ioption(COMMA) RBRACE 
	// 	{ (p, fs, Some b) }	

struct_expr_field: 
	// | oa = outer_attrs id = IDENT COLON e = expr
	// 	{ With_Expr_Ident (None, id, e)}
	// | oa = outer_attrs  t  = tuple_index COLON e = expr 
	// 	{ With_Expr_Tuple_Index (None, t, e)}
	| oa = outer_attrs id = IDENT
	 	{ Without_Expr (None, id) }

struct_base:
	| DOTDOTDOT e = expr { e }

struct_expr_tuple: 
	| p = path_in_expr t = tuple_expr { (p, t) }

struct_expr_unit: 
	| p = path_in_expr { p }

			/* 8.2.9 Call expressions */

call_expr: 
	| e = expr LPAREN ls = separated_list(COMMA, expr) option(COMMA) RPAREN 
	  { (e, ls) }

			/* 8.2.15 Field access expressions */

field_access_expr: 
	| e = expr DOT id = IDENT { (e, id) }

			/* 8.2.15 If and if let expressions */
			
if_expr: 
	| KW_IF e = expr b = block_expr e2 = ioption(else_expr) {	
			(* making sure that expr is not struct_expr *)
			match e with
				| Expr_Without_Block (_, (Struct_Expr _)) -> 
					raise (Parser_Error "Struct expression not allowed in if expression")
				| _ -> (e, b, e2)
		} %prec THEN

else_expr: 
	| KW_ELSE b = block_expr { Else_Block b }
	| KW_ELSE e = if_expr { Else_If e }
	| KW_ELSE e = if_let_expr { Else_If_Let e } 

if_let_expr: 
	| KW_IF KW_LET p = pattern EQ s = scrutinee b = block_expr e = ioption(else_expr) 
	  { (p, s, b, e) } %prec THEN

			/* 8.2.16 Match expressions */

scrutinee: 
	| e = expr {
		(* make sure that the expr is not lazy boolean *) 
		match e with 
			| Expr_Without_Block(_, (Op_Expr (Binary (_, opor, _)))) -> (
				match opor with 
					| Lazy_And | Lazy_Or -> 
						raise (Parser_Error "Lazy boolean operator 
																	not allowed in match expression")
					| _ -> e
				) 
			| _ -> e
	}

/* 12. Names */
	/* 12.6 Visibility and privacy */
visibility: 
	| KW_PUB { Pub }
	| KW_PUB KW_CRATE { Pub_Crate }
	| KW_PUB KW_SUPER { Pub_Super }
	// TODO: Pub_Path


separated_nonempty_list_option_trailing(separator, X):
|  x = X
    { [ x ] }
    [@name one]
| x = X; separator; xs = separated_nonempty_list(separator, X); option(separator)
    { x :: xs }
    [@name more]

separated_list_option_trailing(separator, X):
| xs = loption(separated_nonempty_list_option_trailing(separator, X))
	{ xs }

option_pref_some(X):
  /* nothing */
    { None } %prec OPTION_NONE
    [@name none]
| x = X
    { Some x } 
    [@name some]



/* -2. TODO: not implemented */
type__: 
	| id = IDENT { id }
generic_params: 
	| IMPOSSIBLE_TO_MATCH1 {}
where_clause:
	| IMPOSSIBLE_TO_MATCH2 {}
outer_attrs: 
	| IMPOSSIBLE_TO_MATCH3 {}
inner_attrs: 
	| IMPOSSIBLE_TO_MATCH4 {}
lifetime: 
	| LIFETIME_QUOTE id = IDENT { id }
pattern_no_top_alt: 
	| id = IDENT {id}
path_in_expr: 
	| id = IDENT {id}
pattern: 
	| id = IDENT {id}
type_no_bounds: 
	| id = IDENT {id}