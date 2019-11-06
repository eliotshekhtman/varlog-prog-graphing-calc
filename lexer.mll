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
  | "C" { COMB }
  | "P" { PERM }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "x" { XVAR }
  | "," {COMMA}
  | "Sin" {SIN}
  | "Cos" {COS}
  | "Tan" {TAN}
  | "Arctan" {ARCTAN}
  | "Arccos" {ARCCOS}
  | "Arcsin" {ARCSIN}
  | float { NUM (float_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }