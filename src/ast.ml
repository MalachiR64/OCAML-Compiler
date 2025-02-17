(** type of expressions *)

type bop = Plus | Minus | Times | Leq | And | Or  | Div | Geq | Le | Ge | Eq 

type typ = Tint | Tbool | Tfun of (typ * typ) | Tlist of (typ) 

type expr = Var of string | Integer of int | Boolean of bool | Binop of ( bop * expr * expr)|  If of (expr * expr * expr) | Let of ( string * typ * expr * expr) | Fun of ( string * typ * expr) | App of (expr * expr) | Nil of (typ) | Cons of (expr * expr) | Match of (expr * typ * expr * string * string * expr) 