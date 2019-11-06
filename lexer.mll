{
  open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let float = digit+ '.'? digit*

rule read =
  parse 
  | white { read lexbuf }
  | "EVAL" { EVAL }
  | "GRAPH" { GRAPH }
  | "INTEGRAL" {INTEGRAL}
  | "DERIVATIVE" {DERIVATIVE}
  | "*" { TIMES }
  | "+" { PLUS }
  | "-" { SUBT }
  | "/" { DIV }
  | "^" { POW }
  | "!" { EXCL }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "x" { XVAR }
  | "," {COMMA}
  | float { NUM (float_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }