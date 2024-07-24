%{
    open Syntax 
    open Tokens
%}

// precedence and associativity
// ref: https://doc.rust-lang.org/reference/expressions.html

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

// remove for productions (the start symbols)
%start <item> item
%start <string_literal> string_literal
%start <fn_sig> fn_sig
%%

/* 2. Lexical structure */
    /* 2.6 Tokens */
string_literal: 
    | s = STR_LIT { s }

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
    | sp = ioption(terminated(self_param, COMMA)) fps = separated_nonempty_list(COMMA, fn_param) { (sp, fps) }
// TODO: figure out if it is allowed for list to have a trailing comma
// have to use ioption to make it pass compile

self_param:
    | oa = option(outer_attrs) ss = shorthand_self 
      { Short_Hand(None, ss) }
    | oa = option(outer_attrs) ts = typed_self 
      { Typed_Self(None, ts) }

shorthand_self:
    // | r = option(AND) m = option(KW_MUT) KW_SELFVALUE {Ref_Only(r, m)}
    // | rm = option(pair(AND, lifetime)) m = option(KW_MUT) KW_SELFVALUE 
    //   {Ref_Lifetime(rm, m)}
    | r = option(ref_lifetime) m = option(KW_MUT) KW_SELFVALUE { 
        match r with
        | None -> Ref_Only(None, m)
        | Some (r, Some l) -> Ref_Lifetime(Some (r, l), m)
        | Some (r, None) -> Ref_Only(Some r, m)
     }
ref_lifetime:
    | r = AND { (r, None)}
    | r = AND l = lifetime { (r, Some l) }

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



/* 12. Names */
    /* 12.6 Visibility and privacy */
visibility: 
    | KW_PUB { Pub }
    | KW_PUB KW_CRATE { Pub_Crate }
    | KW_PUB KW_SUPER { Pub_Super }
    // TODO: Pub_Path

/* -2. TODO: not implemented */
type__: 
    | id = IDENT { id }
generic_params: 
    | IMPOSSIBLE_TO_MATCH1 {}
where_clause:
    | IMPOSSIBLE_TO_MATCH2 {}
outer_attrs: 
    | IMPOSSIBLE_TO_MATCH3 {}
lifetime: 
    | LIFETIME_QUOTE id = IDENT { id }
pattern_no_top_alt: 
    | id = IDENT {id}