{
  open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let lletter = ['a'-'z']
let uletter = ['A'-'Z']
let float = digit+ '.'? digit*
let word = (uletter | lletter)+
let string = '"' (word | digit | white)+ '"'
let endchar = '\n'+

rule read =
  parse 
  | string { STRING (Lexing.lexeme lexbuf) }
  | white { read lexbuf }
  | "EVAL" { EVAL }
  | "GRAPH" { GRAPH }
  | "NEWTON" { NEWTON }
  | "EXEC" { EXEC }
  | "INTEGRAL" {INTEGRAL}
  | "DERIVATIVE" {DERIVATIVE}
  | "DISP" { DISP }
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
  | "," {COMMA}
  | "\"" { QUOTE }
  | "SIN" {SIN}
  | "COS" {COS}
  | "TAN" {TAN}
  | "ARCTAN" {ARCTAN}
  | "ARCCOS" {ARCCOS}
  | "ARCSIN" {ARCSIN}
  | float { NUM (float_of_string (Lexing.lexeme lexbuf)) }
  | "VAR" { VAR }
  | ":" { COLON }
  | "END" { END }
  | "IF" { IF }
  | "THEN" { THEN } 
  | "ELSE" { ELSE }
  | endchar { END }
  | word { NAME (Lexing.lexeme lexbuf) }
  | eof { EOF }