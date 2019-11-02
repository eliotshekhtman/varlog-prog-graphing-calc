open OUnit2
open Ast
open Main

(** [make_i n i s] makes an OUnit test named [n] that expects
    [s] to evalute to [Int i]. *)
let make_i n i s =
  n >:: (fun _ -> assert_equal (string_of_float i) (interp s))

let repl_tests = [
  make_i "int" 22. "22";
  make_i "add" 22. "11+11";
  make_i "adds" 22. "(10+1)+(5+6)";
  make_i "mul1" 22. "2*11";
  make_i "mul2" 22. "2+2*10";
  make_i "mul3" 14. "2*2+10";
  make_i "mul4" 40. "2*2*10";
  make_i "sub1" 3. "5-2";
  make_i "div1" 3. "6/2";
  make_i "div2" 1.5 "3/2";
  make_i "pow1" 9. "3^2";
  make_i "neg1" 5. "(-1)+(-1)-(-(4+3))";
  make_i "fact1" 6. "3!";
  make_i "fact2" 123. "(3+2)! + 3"
]

let tests = [
  repl_tests
]

let _ = run_test_tt_main ("suite" >::: repl_tests)
