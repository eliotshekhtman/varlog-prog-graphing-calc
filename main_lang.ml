open Ast 
open Evallang

let parse (s : string) : defn = 
  let lexbuf = Lexing.from_string s in 
  let ast = Parser.parse_defn Lexer.read lexbuf in 
  ast

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
    s |> read_file |> parse |> eval_init; ""
  with 
  | End_of_file -> ""
  | Sys_error _ -> "Error: invalid input: no text file with given name"
  | _ -> "Error: parsing error" 