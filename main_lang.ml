open Ast 

let parse (s : string) : defn = 
  let lexbuf = Lexing.from_string s in 
  let ast = Parser.parse_defn Lexer.read lexbuf in 
  ast

let read_file file = 
  let rec pull_string f ch = 
    match Stdlib.input_line ch with 
    | exception End_of_file -> (close_in ch; "")
    | s -> s ^ pull_string f ch
  in
  let channel = Stdlib.open_in file in 
  let s = pull_string file channel in 
  s |> parse
