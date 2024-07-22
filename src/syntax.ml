
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
*)

exception Not_Implemented of string



(* 1. Lexical structure *)


(* 6. Items *)

type item = 
| Vis_Item of outer_attrs * visibility option * vis_item
| Macro_Item
[@@deriving show, eq]

and 'a non_empty_list = 'a list

and vis_item =
  | Module of module__ (*module is a keyword of OCaml, so use __*)
  | Extern_Crate of extern_crate
  | Use_Declaration of use_decl
  | Fn of fn__
  | Type_Alias of type_alias
  | Struct_vis_item of struct__ (* TODO: figure out on solving name conflicts*)
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
    | Module_Def of safety * string * inner_attribute list * item list

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

and fn__ = 
   fn_qualifiers option * string * generic_params * fn_params option * fn_return_type option * where_clause option * block_expr

and fn_qualifiers = 
  | No_Qualifiers
  | Const
  | Async
  | Unsafe
  | Extern of string option

and fn_params = self_param option * fn_param list 

and self_param = 
  | Short_Hand of outer_attrs * shorthand_self
  | Typed_Self of outer_attrs * typed_self

and shorthand_self = lifetime option * mutability

and typed_self = mutability * type__

and fn_param = 
  | Function_Param_Pattern of outer_attrs * fn_param_pattern
  | Elipsis of outer_attrs
  | Type of outer_attrs * type__

and fn_param_pattern = pattern_no_top_alt * type__ option

and fn_return_type = type__

  (* 6.5 type aliases *)

and type_alias = string * generic_params option * type_param_bounds * where_clause option 
                        * (type__ * where_clause option) option (*type Point = (u8, u8)*)

  (* 6.6 Structs *)

and struct__ = 
  | Struct of struct_struct 
  | Tuple of tuple_struct

and struct_struct = string * generic_params option * where_clause option * struct_fields option

and tuple_struct = string * generic_params option * tuple_fields option * where_clause option

and struct_fields = struct_field non_empty_list

and struct_field = outer_attrs * visibility option * string * type__

and tuple_fields = tuple_field non_empty_list

and tuple_field = outer_attrs * visibility option * type__

  (* 6.7 Enumerations *)

and enum = string * generic_params option * where_clause option * enum_items

and enum_items = enum_item non_empty_list

and enum_item = 
  | Enum_Struct of outer_attrs * visibility option * string * struct_fields * enum_discriminant option
  | Enum_Tuple of outer_attrs * visibility option * string * tuple_fields * enum_discriminant option

and enum_discriminant = expr 


(* 7. Attributes *)

and outer_attr = Not_Implemented    (*#[ attr ]*)
and inner_attribute = Not_Implemented    (*#![ attr ]*)

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
and outer_attrs = outer_attr list

and enumeration = Not_Implemented
and union = Not_Implemented
and constant_item = Not_Implemented
and static_item = Not_Implemented
and trait = Not_Implemented
and implementation = Not_Implemented
and extern_block = Not_Implemented
and generic_params = Not_Implemented
and where_clause = Not_Implemented
and block_expr = Not_Implemented
and lifetime = Not_Implemented
and type__ = string (* TODO: impl *)
and pattern_no_top_alt = Not_Implemented
and type_param_bounds = Not_Implemented
and expr = Not_Implemented