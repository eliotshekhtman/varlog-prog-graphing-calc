open Main 

let rec run () =
  print_string "> ";
  match read_line () with
  | exception (Graphics.Graphic_failure _) -> run ()
  | "" -> run ()
  | "QUIT" -> print_endline "Quitting"
  | "SET_SCALE" -> begin print_endline "x" end

  | s -> begin 
      match interp s with 
      | exception (Failure "no keyword") -> print_endline "Error: no keyword"; run ()
      | exception _ -> 
        print_endline "Error: invalid input"; run ()
      | s -> print_endline s; run ()
    end

let () = run ()