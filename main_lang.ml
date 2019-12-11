open Ast 
open Eval
open VarLog

let parse (s : string) : defn = 
  let lexbuf = Lexing.from_string s in 
  let ast = Parser.parse_defn Lexer.read lexbuf in 
  ast

(** [read_file file] is the text on file [file] *)
let read_file file = 
  let rec pull_string f ch = 
    match Stdlib.input_line ch with 
    | exception End_of_file -> (close_in ch; "")
    | s -> s ^ "\n" ^ pull_string f ch
  in
  let channel = Stdlib.open_in file in 
  let s = pull_string file channel in 
  s


let interp s = 
  try 
    Graphing.open_output_window ();
    s |> read_file |> parse |> eval_init (VarLog.empty ()); ""
  with 
  | End_of_file -> ""
  | Sys_error _ -> "Error: invalid input: no text file with given name"
  | Failure s -> "Failure: " ^ s
  | Stack_overflow -> "Error: Stack Overflow"
  | Invalid_argument s -> "Error: " ^ s
  | _ -> "Error: parsing error" 