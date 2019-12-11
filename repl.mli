(** 
   repl contains the read-eval-print-loop that the user inputs commands into
   and the results of the instructions are output.
*)

(** [run ()] is the actual running repl loop of the program *)
val run: unit -> unit

(** [flex ()] displays the intro description text when make repl is run *)
val flex: unit -> unit