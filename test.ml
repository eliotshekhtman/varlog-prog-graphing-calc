open OUnit2
open Ast
open Main

(** [make_i n i s] makes an OUnit test named [n] that expects
    [s] to evalute to [Int i]. *)
let make_eval_test n i s =
  n >:: (fun _ -> assert_equal i (interp ("EVAL " ^ s)))

let eval_tests = [
  make_eval_test "int" "22" "22";
  make_eval_test "add" "22" "11+11";
  make_eval_test "adds" "22" "(10+1)+(5+6)";
  make_eval_test "mul1" "22" "2*11";
  make_eval_test "mul2" "22" "2+2*10";
  make_eval_test "mul3" "14" "2*2+10";
  make_eval_test "mul4" "40" "2*2*10";
  make_eval_test "sub1" "3" "5-2";
  make_eval_test "div1" "3" "6/2";
  make_eval_test "div2" "1.5" "3/2";
  make_eval_test "pow1" "9" "3^2";
  make_eval_test "neg1" "5" "(-1)+(-1)-(-(4+3))";
  make_eval_test "fact1" "6" "3!";
  make_eval_test "fact2" "123" "(3+2)! + 3";
  make_eval_test "int1" "4.00000099995" "INTEGRAL((1,3),x)";
  make_eval_test "int2" "15.0000009999" "1+3!+INTEGRAL((1,3),x)+4";
  make_eval_test "sin1" "0.841470984808" "SIN 1";
  make_eval_test "sin2" "5.00000979345" "1 + SIN(3+8) + 5";
]

let tests = [
  eval_tests
]

let _ = run_test_tt_main ("suite" >::: eval_tests)
