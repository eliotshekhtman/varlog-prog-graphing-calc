{
  open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let float = '-'? digit+ '.'? digit*

rule read =
  parse 
  | white { read lexbuf }
  | "*" { TIMES }
  | "+" { PLUS }
  | "-" { SUBT }
  | "/" { DIV }
  | "^" { POW }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | float { NUM (float_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }