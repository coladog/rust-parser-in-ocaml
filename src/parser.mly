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

item:
//TODO: ignoring all attributes at this moment, should fix it
    | v = option(visibility) vi = vis_item { Vis_Item([], v, vi) }

visibility: 
    | KW_PUB { Pub }
    | KW_PUB KW_CRATE { Pub_Crate }
    | KW_PUB KW_SUPER { Pub_Super }
    // TODO: Pub_Path

vis_item: 
    | s = struct__ { Struct_vis_item(s) }

struct__:
    | ss = struct_struct { Struct(ss) }
    | ts = tuple_struct { Tuple(ts) }

struct_struct: 
// TODO: add generic_params, where_clause
// struct_struct = string * generic_params option * where_clause option * struct_fields option
    | KW_STRUCT id = IDENT LBRACE sf = option(struct_fields) RBRACE { (id, None, None, sf) } 
tuple_struct: 
// TODO: add generic_params, where_clause
// tuple_struct = string * generic_params option * tuple_fields option * where_clause option
    | KW_STRUCT id = IDENT LPAREN tf = option(tuple_fields) RPAREN { (id, None, tf, None) }

struct_fields:
    | sf = struct_field option(COMMA) {[sf]}
    | sf = struct_field COMMA sfs = struct_fields {sf :: sfs}

struct_field: 
// TODO: add outer_attrs and actual type 
    | v = option(visibility) id = IDENT COLON tid = IDENT { ([], v, id, tid) }

tuple_fields:
    | tf = tuple_field option(COMMA) {[tf]}
    | tf = tuple_field COMMA tfs = tuple_fields {tf :: tfs}

tuple_field: 
// TODO: add outer_attrs and actual type 
    | v = option(visibility) tid = IDENT { ([], v, tid) }
