open OUnit2
open Interp
open Ast
open Main

let make_t n i s =
  [n >:: (fun _ -> assert_equal (Integer i) (typ_eval s))]

let make_e n i s =
  [n >:: (fun _ -> assert_raises i (fun () -> typ_eval s))]

  let tests = [
    make_t "neg_int" (-22) "-22";
    make_t "sub" 5 "15-10";
    make_t "nested_add" 30 "(3+4) + (5+6) + (7+8)";
    make_t "nested_let" 50 "let a:int = 10 in let b:int = 5 in a * (b + 2)";
    make_t "let_reuse" 9 "let x:int = 3 in let x:int = x+2 in x * x";
    make_t "mult_div" 10 "100/5/2";
    make_t "nested_mult" 48 "(2*3) * (4*2)";
    make_t "mult_priority" 22 "2+10*2";
    make_t "comparison_leq" 1 "if 5<=10 then 1 else 0";
    make_t "comparison_ge" 0 "if 3>4 then 1 else 0";
    make_t "comparison_eq" 1 "if 10=10 then 1 else 0";
    make_t "nested_condition" 5 "if (10>5) && (5<7) then 5 else 0";
    make_t "or_condition" 10 "if false || (4<7) then 10 else 0";
    make_t "complex_if" 99 "if (3+2) <= (5+1) then 99 else 0";
    make_t "let_in_if" 30 "let y:bool = ((5+1) > (3+3)) in if y then 30 else 0";
    make_t "function_apply" 15 "(fun x:(int) -> x*3) 5";
    make_t "let_function" 7 "let f:(int -> int) = (fun x:(int) -> x+2) in f 5";
    make_t "function_nested" 200 "((fun x:(int -> int) -> fun y:(int) -> x(y)) (fun x:(int) -> x*2)) 100";
    make_t "list_head" 10 "let lst:int list = 10::20::30::([]:(int list)) in match lst with [] -> -1 | hd::tl -> hd";
    make_t "list_match_empty" (-1) "let lst:int list = []:(int list) in match lst with [] -> -1 | hd::tl -> hd";
    make_t "list_match_tail" 20 "let lst:int list = 10::20::30::([]:(int list)) in match lst with [] -> -1 | hd::tl -> match tl with [] -> -1 | hd2::tl2 -> hd2";
    make_t "boolean_list" 1 "if (let x:bool list = true::false::[]:(bool list) in match x with [] -> false | hd::tl -> hd) then 1 else 0";
    make_t "double_function" 24 "let f:(int -> int) = (fun x:(int) -> x*2) in let g:(int -> int) = (fun x:(int) -> x+4) in f (g 10)";
    make_t "list_nested_match" 50 "let x:int list = 10::20::30::[] in match x with [] -> -1 | hd::tl -> match tl with [] -> -1 | hd2::tl2 -> match tl2 with [] -> -1 | hd3::tl3 -> hd3 * 2";
    make_e "add_bool" Interp.Main.Nottyped "true + 1";
    make_e "bool_if_mismatch" Interp.Main.Nottyped "if 5 then 1 else 0";
    make_e "wrong_list_match" Interp.Main.Nottyped "match []:(bool list) with [] -> 0 | hd::tl -> hd";
    make_e "fun_type_mismatch" Interp.Main.Nottyped "(fun x:(bool) -> x+1) true";
    make_e "nested_fun_error" Interp.Main.Nottyped "((fun x:(int -> int) -> fun y:(bool) -> x y) (fun x:(int) -> x+2)) true";
    make_e "list_type_error" Interp.Main.Nottyped "let y:int list = true::[]:(int list) in match y with [] -> 0 | hd::tl -> hd";
    make_e "app_type_mismatch" Interp.Main.Nottyped "(fun x:(int) -> x*2) true";

  ]