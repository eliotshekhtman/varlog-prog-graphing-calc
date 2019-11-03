open Main 

let rec run () =
  print_string "> ";
  match read_line () with
  | "" -> run ()
  | "QUIT" -> print_endline "Quitting"
  | s -> begin 
      match interp s with 
      | exception (Match_failure _) -> print_endline "Error: no keyword"; run ()
      | exception _ -> 
        print_endline "Error: invalid input"; run ()
      | s -> print_endline s; run ()
    end

let () = run ()