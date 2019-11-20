open Main 
open Graphing

let rec run () =
  print_string "> ";
  match read_line () with
  | exception (Graphics.Graphic_failure _) -> print_endline ""; print_endline "Quitting"
  | "" -> run ()
  | "QUIT" -> print_endline "Quitting"
  | "SET_SCALE" -> begin print_endline "x" end

  | s -> begin 
      match interp s with 
      | exception No_keyword -> print_endline "Error: no keyword"; run ()
      | exception _ -> 
        print_endline "Error: invalid input"; run ()
      | s -> print_endline s; run ()
    end

let flex () = 
  print_endline "------------ V/\\RL0G -------------";
  print_endline "[EVAL e] to evaluate an expression";
  print_endline "[GRAPH f] to graph a function";
  print_endline "  (implicitly set to y)";
  print_endline "  Do not close the graphing window";
  print_endline "[NEWTON n] to evaluate polynomial";
  print_endline "  function of 1 variable, 0<=n<=5";
  print_endline "[EXEC fn] to execute a text file;";
  print_endline "  don't include file extension";
  print_endline "----------------------------------"; ()

let () = 
  graph_func (-10.) 10. (-10.) 10. 0 (fun x -> 100000.);
  flex ();
  run ()