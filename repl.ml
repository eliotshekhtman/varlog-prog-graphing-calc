open Main 
open Graphing
open Printexc
open ANSITerminal

let rec run () =
  Stdlib.print_string "> "; 
  match read_line () with
  | exception (Graphics.Graphic_failure _) -> 
    print_endline ""; 
    print_endline "Quitting"
  | "" -> run ()
  | "QUIT" -> print_endline "Quitting"
  | "SET_SCALE" -> begin print_endline "x" end

  | s -> begin 
      match interp s with 
      | exception No_keyword -> print_endline "Error: no keyword"; run ()
      | exception Main.DeterminantZero  ->
        print_endline "Can't solve: infinite solutions or none"; run ()
      | exception Division_by_zero ->
        print_endline "Division by 0 exc"; run ()
      | exception (Failure s) -> print_endline ("Failure: " ^ s); run ()
      | exception _ -> 
        print_endline "Error: invalid input"; run ()  
      | s -> print_endline s; run ()
    end

let flex () = 
  print_endline "------------ V/\\RL0G -------------";
  print_endline "[GRAPH f] to graph a function";
  print_endline "  (implicitly set to y)";
  print_endline "  Do not close the graphing window";
  print_endline "[SETSCALE] to change the scaling";
  print_endline "  of future graphs";
  print_endline "[SOLVE (2x2 or 3x3)] to evaluate a";
  print_endline "  system of 2 or 3 linear eqs of";
  print_endline "  2 or 3 variables, respectively";
  print_endline "[NEWTON f] to evaluate a function";
  print_endline "  of 1 variable, using guesses";
  print_endline "[EXEC fn] to execute a text file;";
  print_endline "  don't include file extension";
  print_endline "----------------------------------"; ()

(** Allows the repl to run automatically when make repl is called; creates
    a default open graph window from the start so it'd make sense that 
    closing it would end the program *)
let () = 
  graph (-10.) 10. (-10.) 10. (fun x -> 100000.);
  flex ();
  run ()