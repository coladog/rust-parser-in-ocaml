type node = 
  | Node of string * node list 
[@@deriving show]