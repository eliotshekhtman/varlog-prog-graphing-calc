open OUnit2
open Ast
open Main
open Eval

(* We are not testing I/O expressions/etc ([GETKEY], [PROMPT], [SOLVER], 
    [GRAPHING] etc), as they would interrupt the flow of the testing: 
     as such, we take the proper working of our repl tests and 
     execution of our text files which use them as proof of their 
     proper behavior.  This includes graphing, the solvers (Newton 
     and linear), and complex script files.  We only test small 
     scripts to ensure that they compile, but can't test their actual
     side effects, except by analysis.  We assume that their compilation,
     combined with our inspection, will ensure their scalability into 
     larger examples.  
     Through OUnit, we mainly focused on testing things as they'd appear in
     the REPL, so eval.ml is mainly tested, with other functions from
     every other module coming into play (except modules State and Coords in
     Main).  They were developed mainly through glass-box style, focusing
     on covering all branches of the pattern matches; however, 
     the manual testing delved into both as we attempted to ensure correctness. 

     We posit that this testing approach demonstrates correctness of the system,
     as we test almost all branches made available to us through the test
     cases and cover the rest in manual testing.  All evaluations are guaranteed
     to work as expected, and simple combinations of definitions are too, with 
     complex examples being integrated in our complex test files. (aka the 
     files with the scripting in our language: connectfour.vl, testLOOP.vl, 
     chess_obj.vl, etc)

*)

(** [make_eval_test n i s] makes an OUnit test named [n] that expects
    [s] to evalute to [String i]. *)
let make_eval_test n i s =
  n >:: (fun _ -> assert_equal i (interp s) ~printer:(fun (s:string) -> s))

(** [make_exec_test n i s] makes an OUnit test named [n] that 
    expects file [s] to evaluate to [String i] *)
let make_exec_test n i s = 
  n >:: (fun _ -> assert_equal i (interp ({|EXEC "|} ^ s ^ {|"|}) ) 
            ~printer:(fun (s:string) -> s))


let matrix = [| [|0.;4.;0.|]; [|0.;0.;7.|] ; [|0.;0.;0.|] |] 
let matrix2 = [| [|1.;4.;0.|]; [|0.;1.;7.|] ; [|0.;0.;1.|] |] 

let eval_tests = [
  make_eval_test "integer" "22" "22";
  make_eval_test "float" "23.5" "23.5";
  make_eval_test "string" "Hello World" {|"Hello World"|};
  make_eval_test "bool" "true" "true";
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
  make_eval_test "eq1" "true" "1 = 1";
  make_eval_test "eq2" "false" "1 = 2";
  make_eval_test "eq3" "false" "2 = 1";
  make_eval_test "eq4" "true" "true = true";
  make_eval_test "eq5" "false" "true = false";
  make_eval_test "eq6" "true" {|"hello" = "hello"|};
  make_eval_test "neq1" "false" "1 != 1";
  make_eval_test "neq2" "true" "2 != 1";
  make_eval_test "neq3" "true" "1 != 2";
  make_eval_test "neq4" "true" "1 != true";
  make_eval_test "neq5" "false" "true != true";
  make_eval_test "eq7" "false" {|"hello" = "hi"|};
  make_eval_test "lt1" "true" "1 < 2";
  make_eval_test "lt2" "true" "1 < 1.2";
  make_eval_test "lt3" "true" "false < true";
  make_eval_test "lt4" "false" "1 < 1";
  make_eval_test "gt1" "true" "2 > 1";
  make_eval_test "gt2" "false" "2 > 2";
  make_eval_test "gt3" "false" "2 > 3";
  make_eval_test "leq1" "true" "1 <= 2";
  make_eval_test "leq2" "true" "2 <= 2";
  make_eval_test "leq3" "false" "3 <= 2";
  make_eval_test "geq1" "true" "2 >= 1";
  make_eval_test "geq2" "true" "1 >= 1";
  make_eval_test "geq3" "false" "1 >= 2";
  make_eval_test "and1" "true" "true & true";
  make_eval_test "and2" "false" "true & false";
  make_eval_test "and3" "false" "false & true";
  make_eval_test "and4" "false" "false & false";
  make_eval_test "and5" "true" "1=1 & true";
  make_eval_test "and6" "true" "1=1 & 2=2";
  make_eval_test "and7" "false" "1 != 1 & 2=2";
  make_eval_test "and8" "false" "1 != 1 & 2 != 2";
  make_eval_test "and9" "true" "1+1 = 2 & 3+2 = 5/1";
  make_eval_test "and10" "true" "true AND true";
  make_eval_test "and11" "false" "true AND false";
  make_eval_test "or1 tt" "true" "true | true";
  make_eval_test "or2 tf" "true" "true | false";
  make_eval_test "or3 ft" "true" "false | true";
  make_eval_test "or4 ff" "false" "false | false";
  make_eval_test "or5 ft expr" "true" "false | 1 = 1";
  make_eval_test "or6 OR tt" "true" "true OR true";
  make_eval_test "or7 OR tf" "true" "true OR false";
  make_eval_test "nand1 tt" "false" "true !& true";
  make_eval_test "nand2 NAND tt" "false" "true NAND true";
  make_eval_test "nand3 tf" "true" "true !& false";
  make_eval_test "nand4 NAND tf" "true" "true NAND false";
  make_eval_test "nand5 ft" "true" "false !& true";
  make_eval_test "nand6 NAND ft" "true" "false NAND true";
  make_eval_test "nand7 ff" "true" "false !& false";
  make_eval_test "nor1 ff" "true" "false !| false";
  make_eval_test "nor2 NOR ff" "true" "false NOR false";
  make_eval_test "nor3 tt" "false" "true !| true";
  make_eval_test "nor4 ft" "false" "false !| true";
  make_eval_test "nor5 tf" "false" "true !| false";
  make_eval_test "nor6 NOR tt" "false" "true NOR true";
  make_eval_test "nor6 ft expr" "false" "1=2 !| 1=1";
  make_eval_test "xor1 tt" "false" "true XOR true";
  make_eval_test "xor2 tf" "true" "true XOR false";
  make_eval_test "xor3 ft" "true" "false XOR true";
  make_eval_test "xor4 ff" "false" "false XOR false";
  make_eval_test "xor5 tf expr" "true" "1=1 XOR 1=2";
  make_eval_test "nxor1 tt" "true" "true NXOR true";
  make_eval_test "nxor2 tf" "false" "true NXOR false";
  make_eval_test "nxor3 ft" "false" "false NXOR true";
  make_eval_test "nxor4 ff" "true" "false NXOR false";
  make_eval_test "rint1 lt" "true" "RANDINT(0,4) < 4";
  make_eval_test "rint2 gt" "true" "RANDINT(0,4) > -1";


  make_eval_test "function check application" "5" 
    "FUN hello: a b -> {x:a END RETURN x} END x: hello<-(5 6) END RETURN x";

  make_eval_test "function check closure test" "<closure>" 
    "FUN hello: a b -> {x:a END RETURN x} END x: hello<-(5 6) END RETURN hello";

  make_eval_test "function scalar mult matrix test" "30" 
    "x : MATRIX(3,4) END x[2,2] : 5
     FUN scalar: b -> {RETURN b * x} END y: scalar<-(6) END RETURN y[2,2]";


  make_eval_test "function standard matrix mult test" (Eval.print_matrix matrix)
    "x : MATRIX(3,3) END x[0,0] : 1 END x[1,1] : 1 END x[2,2] : 1 END DISP x
     y : MATRIX(3,3) END y[0,1] : 4 END y[1,2] : 7 END DISP y
     FUN matMult: mOne mTwo -> {RETURN mOne * mTwo} 
     z: matMult<-(x y) END RETURN z";

  make_eval_test "function standard matrix add test" (Eval.print_matrix matrix2)
    "x : MATRIX(3,3) END x[0,0] : 1 END x[1,1] : 1 END x[2,2] : 1 END DISP x
      y : MATRIX(3,3) END y[0,1] : 4 END y[1,2] : 7 END DISP y
      FUN matAdd: mOne mTwo -> {RETURN mOne + mTwo} 
      z: matAdd<-(x y) END RETURN z";

  make_eval_test "function1" "Bagel" 
    "FUN bagel :-> { RETURN \"Bagel\" } 
     RETURN bagel<-()";

  make_exec_test "disp" "" "testDISP";
  make_eval_test "struct1" "" "STRUCT hello : a -> { x : a }";
  make_eval_test "struct2" "<struct>" 
    "STRUCT hello : a -> { x : a } END RETURN hello";
  make_eval_test "struct3" "" 
    "STRUCT hello : a -> { x : a } END greet : hello<-(3)";
  make_eval_test "struct4" "<built>" 
    "STRUCT hello : a -> { x : a } END greet : hello<-(3) END RETURN greet";
  make_eval_test "struct5" "3" 
    "STRUCT hello : a -> { x : a } END greet : hello<-(3) END RETURN greet$x";
  make_eval_test "struct6" "4" 
    "STRUCT hello : a -> { x : a } 
     greet : hello<-(3) END greet$x : 4 END RETURN greet$x";

  make_eval_test "varmat1" "hello" 
    "x : VARMAT(3,3) END x[1,1] : \"hello\"
     RETURN x[1,1]";

  make_exec_test "disp" "" "testDISP";
  make_exec_test "goto" "" "testGOTO";
  make_exec_test "random" "" "testRANDOM";
  make_exec_test "goto2" "" "testWHILE";


  (* Post-CS3110 tests *)
  make_eval_test "objvar1" "4" 
    "CLASS hello : a -> { DISP \"hi\" }
     greet : hello<-(3)
     greet.a : 4
     RETURN greet.a";
  make_eval_test "objfunc1" "3" 
    {|CLASS hello : a -> { 
        FUN val :-> { RETURN a }
      }
      greet : hello<-(3)
      RETURN greet.val<-()|};
  make_eval_test "objfunc2" "4" 
    {|CLASS hello : a -> { 
        FUN val :-> { RETURN a+1 }
      }
      greet : hello<-(3)
      RETURN greet.val<-()|};
  make_eval_test "objfunc3" "4" 
    {|CLASS hello : a -> { 
        FUN val :-> { RETURN a }
      }
      greet : hello<-(3)
      greet.a : 4
      RETURN greet.val<-()|};
  make_eval_test "objfunc4" "4" 
    {|CLASS hello : a -> { 
        FUN val :-> { a : a+1 }
      }
      greet : hello<-(3)
      greet.val<-()
      RETURN greet.a|};
  make_eval_test "funcvar1" "4"
    {|a : 3
      FUN f :-> { a : a+1 }
      f<-()
      RETURN a|};
]

let tests = [
  eval_tests
]

let _ = run_test_tt_main ("suite" >::: eval_tests)
