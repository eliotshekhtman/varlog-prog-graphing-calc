{
  open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let lletter = ['a'-'z']
let uletter = ['A'-'Z']
let float = digit+ '.'? digit*
let word = [uletter lletter]+
let string = '"' word '"'

rule read =
  parse 
  | white { read lexbuf }
  | "*" { TIMES }
  | "+" { PLUS }
  | "-" { SUBT }
  | "/" { DIV }
  | "^" { POW }
  | "!" { EXCL }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | float { NUM (float_of_string (Lexing.lexeme lexbuf)) }
  | "VAR" { VAR }
  | "END" { END }
  | word { NAME (Lexing.lexeme lexbuf) }
  | eof { EOF }