(* AST structs refer to https://github.com/rust-lang/reference/tree/b1d10606478677491effea5880ca80d97fccca4c *)
(* Naming convention refers to https://github.com/lindig/ocaml-style*)
(* 
Types: lower_case.
Type variables: 'lower_case
Values and Functions: lower_case
Constructors: Upper_Case
Record Fields: lower_case
Modules: UpperCase
Signatures: UpperCase
Module Types: ALLCAPS 
*)
(* 
Term short forms:

function -> fn 
attribute -> attr
declaration -> decl
expression -> expr
enumeration -> enum
statement -> stmt
*)
(* Trying to support all features in https://github.com/SLMT/rust-snake/tree/master/src *)



(* 2. Lexical structure *)
(* 2.6 Tokens *) 
type string_literal = string
[@@deriving show, eq]
type byte_literal = string
[@@deriving show, eq]
type integer_literal =
	| Dec_Int_Lit of string
	| Bin_Int_Lit of string
	| Oct_Int_Lit of string
	| Hex_Int_Lit of string
[@@deriving show, eq]
type float_literal = string
[@@deriving show, eq]
	(* 6. Items *)
	
type item = 
	| Vis_Item of outer_attrs option * visibility option * vis_item
	| Macro_Item
	[@@deriving show, eq]
	
and 'a non_empty_list = 'a list
and not_implemented = string

and vis_item =
	| Module of module__ (*module is a keyword of OCaml, so use __*)
	| Extern_Crate of extern_crate
	| Use_Declaration of use_decl
	| Fn of fn
	| Type_Alias of type_alias
	| Struct_Vis_Item of struct__ (* TODO: figure out on solving name conflicts*)
	| Enumeration of enumeration
	| Union of union
	| Constant_Item of constant_item
	| Static_Item of static_item
	| Trait of trait 
	| Implementation of implementation
	| Extern_Block of extern_block
	
	(* 6.1 Modules *)

and module__ =   
	| Module_Decl of safety * string
	| Module_Def of safety * string * inner_attrs option * item list

	(* 6.2 Extern crate declarations *)

and extern_crate = crate_ref * as_clause option

and crate_ref = 
	| Identifier of string
	| Self

and as_clause = 
	| Identifier of string 
	| Underscore

	(* 6.3 Use declarations *)

and use_decl = 
	| End_With_Asterisk of simple_path option            (*use std::io::*;*)
	| Use_Multiple of simple_path option * use_decl list (*use std::io::{self, Write};*)
	| Use_Single of simple_path * as_clause option       (*use std::io::Write; use std::io::Write as w*)

	(* 6.4 Functions *)

and fn = fn_sig * block_expr
	[@@deriving show, eq]

and fn_sig = fn_qualifiers option * string * generic_params option * 
						 fn_params option * fn_return_type option * where_clause option

and fn_qualifiers = 
	| Const
	| Async
	| Unsafe
	| Extern of fn_abi option

and fn_abi = string_literal

and fn_params = self_param option * fn_param list 

and self_param = 
	| Short_Hand of outer_attrs option * shorthand_self
	| Typed_Self of outer_attrs option * typed_self

and shorthand_self =
	| Ref_Only of reference option * mutability
	| Ref_Lifetime of (reference * lifetime) option * mutability

and typed_self = mutability * type__

and fn_param = 
	| Fn_Param_Pattern of outer_attrs option * fn_param_pattern
	| Type_Or_Elipsis of outer_attrs option * type_or_elipsis

and fn_param_pattern = pattern_no_top_alt * type_or_elipsis

and type_or_elipsis = 
	| Type of type__
	| Elipsis

and fn_return_type = type__

	(* 6.5 type aliases *)

and type_alias = string * generic_params option * type_param_bounds * 
								 where_clause option * (type__ * where_clause option) option 
								 (*type Point = (u8, u8)*)

	(* 6.6 Structs *)

and struct__ = 
	| Struct of struct_struct 
	| Tuple of tuple_struct

and struct_struct = string * generic_params option * where_clause option * 
										struct_fields option

and tuple_struct = string * generic_params option * tuple_fields option * 
									 where_clause option

and struct_fields = struct_field non_empty_list

and struct_field = outer_attrs option * visibility option * string * type__

and tuple_fields = tuple_field non_empty_list

and tuple_field = outer_attrs option * visibility option * type__

	(* 6.7 Enumerations *)

and enum = string * generic_params option * where_clause option * enum_items

and enum_items = enum_item non_empty_list

and enum_item = 
	| Enum_Struct of outer_attrs option * visibility option * string * 
									 struct_fields * enum_discriminant option
	| Enum_Tuple of outer_attrs option * visibility option * string * 
									tuple_fields * enum_discriminant option

and enum_discriminant = expr 


(* 7. Attributes *)

and outer_attr = not_implemented    (*#[ attr ]*)
and inner_attr = not_implemented    (*#![ attr ]*)

(* 8. Statements and expressions*)
	(* 8.1 Statements *)

and stmt = 
	| Semi
	| Item of item 
	| Let_Stmt of let_stmt
	| Expr_Stmt of expr_stmt
	| Macro_Invocation_Semi of macro_invocation_semi

and let_stmt = outer_attrs option * pattern_no_top_alt * type__ option * (expr 
							non_empty_list * block_expr option) option


and expr_stmt = 
	| Expr_Without_Block_Stmt of expr_without_block
	| Expr_With_Block_Stmt of expr_with_block

and macro_invocation_semi = not_implemented

(* 8.2 Expressions *)

and expr = 
	| Expr_Without_Block of outer_attrs option * expr_without_block
	| Expr_With_Block of outer_attrs option * expr_with_block

and expr_without_block = 
	| Literal_Expr of literal_expr
	| Op_Expr of op_expr
	| Array_Expr of array_expr 
	| Tuple_Expr of tuple_expr
	| Tuple_Index_Expr of tuple_index_expr
	| Struct_Expr of struct_expr
	| Call_Expr of call_expr
	| Field_Access_Expr of field_access_expr

and expr_with_block = 
	| Block_Expr of block_expr
	| If_Expr of if_expr
	| If_Let_Expr of if_let_expr
	| Unsafe_Block_Expr of unsafe_block_expr

		(* 8.2.1 Literal expressions *)

and literal_expr = 
	| String_Literal of string_literal
	| Byte_Literal of byte_literal
	| Integer_Literal of integer_literal
	| Float_Literal of float_literal
	(* TODO: complete all literals: https://doc.rust-lang.org/reference/expressions/literal-expr.html*)

		(* 8.2.2 Path expressions *)

		(* 8.2.3 Block expressions *)
		
and block_expr = inner_attrs option * stmt list * expr_without_block option

and async_block_expr = move option * block_expr

and const_block_expr = block_expr

and unsafe_block_expr = block_expr


			(* 8.2.4 Operator expressions *)

			(* Not completely the same as the structure in https://doc.rust-lang.org/reference/expressions/operator-expr.html *)
and bin_opor = 
	| Add | Sub | Mul | Div | Rem | Bit_And | Bit_Or | BitXor | Shl | Shr
	| Eq | Ne | Lt | Le | Gt | Ge
	| Lazy_Or | Lazy_And
	| Assign | Add_Assign | Sub_Assign | Mul_Assign | Div_Assign | Rem_Assign
	| And_Assign | Or_Assign | Xor_Assign | Shl_Assign | Shr_Assign

and un_opor = 
	| Neg | Not | Deref | Ref | Mut_Ref | Error_Propagation

and op_expr = 
	| Unary of un_opor * expr
	| Binary of expr * bin_opor * expr
	| Cast of expr * type_no_bounds

			(* 8.2.5 Grouped expressions *)
			(* skiped, just add parathensis around *)

			(* 8.2.6 Array and index expression *)

and array_expr = 
	| Exprs of expr list
	| Repeat of expr * expr

and index_expr = expr * expr
			
			(* 8.2.7 Tuple and index expression *)

and tuple_expr = expr list 

and tuple_index = integer_literal

and tuple_index_expr = expr * tuple_index

			(* 8.2.8 Struct expressions *)

and struct_expr = 
	| Struct_Expr_Struct of struct_expr_struct
	| Struct_Expr_Tuple of struct_expr_tuple
	| Struct_Expr_Unit of struct_expr_unit

and struct_expr_struct = path_in_expr * struct_expr_field list * struct_base option

and struct_expr_field = 
	| With_Expr_Ident of outer_attrs option * string * expr
	| With_Expr_Tuple_Index of outer_attrs option * tuple_index * expr
	| Without_Expr of outer_attrs option * string 

and struct_base = expr

and struct_expr_tuple = path_in_expr * expr list 

and struct_expr_unit = path_in_expr

			(* 8.2.9 Call expressions *)

and call_expr = expr * expr list

			(* 8.2.10 Method call expressions *)

			(* TODO *)

			(* 8.2.11 Field access expressions *)

and field_access_expr = expr * string

			(* 8.2.15 If and if let expressions *)

and if_expr = expr * block_expr * else_expr option
(* note that the first expr cannot be struct expr *)
	
and else_expr = 
	| Else_block of block_expr 
	| Else_If of if_expr 
	| Else_If_Let of if_let_expr

and if_let_expr = pattern * scrutinee * block_expr * else_expr option
(* except lazy boolean operator exprs for scrutinee *)

			(* 8.2.16 Match expressions *)

and scrutinee = expr (* expr except struct expression *)

(* 9. Patterns *)

and pattern = not_implemented

(* 10. Type system *)


(* 12. Names *)
	(* 12.4 Paths*)

and simple_path = 
	| Start_With_Double_Collon of simple_path_segment list (*::std::io*)
	| Start_Without_Double_Collon of simple_path_segment list (*std::io*)

and simple_path_segment =
	| Identifier of string 
	| Super
	| Self
	| Crate
	| Dollar_Sign_Crate

	(* 12.6 Visibility and privacy *)
and visibility = 
	| Pub
	| Pub_Crate
	| Pub_Super
	| Pub_Path of simple_path


and unsafe = unit
and safety = unsafe option
and mut = unit
and mutability = mut option
and reference = unit
and move = unit
and outer_attrs = outer_attr list (* an empty list is #[], a None is just nothing *)
and inner_attrs = inner_attr list
and enumeration = not_implemented
and union = not_implemented
and constant_item = not_implemented
and static_item = not_implemented
and trait = not_implemented
and implementation = not_implemented
and extern_block = not_implemented
and generic_params = not_implemented
and where_clause = not_implemented
and lifetime = not_implemented
and type__ = not_implemented
and type_no_bounds = not_implemented
and pattern_no_top_alt = not_implemented
and type_param_bounds = not_implemented
and path_in_expr = not_implemented