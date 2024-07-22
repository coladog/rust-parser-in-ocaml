%{
    open Syntax 
    open Tokens
%}

// precedence and associativity
// ref: https://doc.rust-lang.org/reference/expressions.html

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

%start <item> item

%%

/* 6. Items */
item:
//TODO: ignoring all attributes at this moment, should fix it
    | oa = option(outer_attrs) v = option(visibility) vi = vis_item { Vis_Item(None, v, vi) }


vis_item: 
    | s = struct__ { Struct_Vis_Item(s) }


    /* 6.6 Structs */
struct__:
    | ss = struct_struct { Struct(ss) }
    | ts = tuple_struct { Tuple(ts) }

struct_struct: 
// TODO: add generic_params, where_clause
// struct_struct = string * generic_params option * where_clause option * struct_fields option
    | KW_STRUCT id = IDENT gp = option(generic_params) wc = option(where_clause) LBRACE sf = option(struct_fields) RBRACE { (id, None, None, sf) } 
    | KW_STRUCT id = IDENT gp = option(generic_params) wc = option(where_clause) SEMI { (id, None, None, None) } // unit struct
tuple_struct: 
// TODO: add generic_params, where_clause
// tuple_struct = string * generic_params option * tuple_fields option * where_clause option
    | KW_STRUCT id = IDENT gp = option(generic_params) LPAREN tf = option(tuple_fields) RPAREN wc = option(where_clause) SEMI { (id, None, tf, None) }

struct_fields:
    | sf = struct_field option(COMMA) {[sf]}
    | sf = struct_field COMMA sfs = struct_fields {sf :: sfs}

struct_field: 
// TODO: add outer_attrs and actual type 
    | oa = option(outer_attrs) v = option(visibility) id = IDENT COLON tid = type__ { (None, v, id, tid) }

tuple_fields:
    | tf = tuple_field option(COMMA) {[tf]}
    | tf = tuple_field COMMA tfs = tuple_fields {tf :: tfs}

tuple_field: 
// TODO: add outer_attrs and actual type 
    | oa = option(outer_attrs) v = option(visibility) tid = type__ { (None, v, tid) }

/* 12. Names */
    /* 12.6 Visibility and privacy */
visibility: 
    | KW_PUB { Pub }
    | KW_PUB KW_CRATE { Pub_Crate }
    | KW_PUB KW_SUPER { Pub_Super }
    // TODO: Pub_Path

/* -1. TODO: not implemented */
type__: 
    | id = IDENT { id }
generic_params: 
    | IMPOSSIBLE_TO_MATCH {}
where_clause:
    | IMPOSSIBLE_TO_MATCH {}
outer_attrs: 
    | IMPOSSIBLE_TO_MATCH {}