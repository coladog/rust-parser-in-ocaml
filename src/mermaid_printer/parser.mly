%{
open Syntax
%}

%token LPAREN RPAREN EOF COMMA SEMI LBRACK RBRACK
%token <string> IDENT 
%start <node> node_toplevel

%nonassoc LOW_PREC
%nonassoc LPAREN RPAREN COMMA SEMI LBRACK RBRACK IDENT
%nonassoc EOF
%%

node_toplevel: 
    |  n = node EOF { n }

node: 
    | id = IDENT LPAREN ns = separated_nonempty_list(COMMA, node) RPAREN 
        { Node (id, ns) } %prec LOW_PREC
    | id = IDENT n = node { Node (id, [n]) }
    | id = IDENT { Node (id, []) }
    | l = list_of_nodes { Node ("_list_", l) }
    | t = tuple_of_nodes { Node ("_tuple_", t) }
    | LPAREN n = node RPAREN { n }

list_of_nodes: 
    | LBRACK ns = separated_list(SEMI, node) RBRACK { ns }
tuple_of_nodes:
    | LPAREN ns = separated_nonempty_list(COMMA, node) RPAREN { ns }



