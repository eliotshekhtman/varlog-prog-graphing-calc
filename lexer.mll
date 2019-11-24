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
let endchar = '\n'*

rule read =
  parse 
  | string { STRING (Lexing.lexeme lexbuf) }
  | white { read lexbuf }
  | "EVAL" { EVAL }
  | "GRAPH" { GRAPH }
  | "NEWTON" { NEWTON }
  | "SOLVE" {SOLVE}
  | "EXEC" { EXEC }
  | "SETSCALE" { SETSCALE }
  | "INTEGRAL" {INTEGRAL}
  | "DERIVATIVE" {DERIVATIVE}
  | "DISP" { DISP }
  | "GETKEY" { GETKEY }
  | "PROMPT" { PROMPT }
  | "OUTPUT" { OUTPUT }
  | "RANDINT" { RANDINT }
  | "<-" { RARROW }
  | "[" { LBRACKET }
  | "]" { RBRACKET}
  | "->" { ARROW }
  | "*" { TIMES }
  | "+" { PLUS }
  | "-" { SUBT }
  | "/" { DIV }
  | "^" { POW }
  | "C" { COMB }
  | "P" { PERM }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "," {COMMA}
  | "<=" { LEQ }
  | ">=" { GEQ }
  | "!=" { NEQ }
  | "=" { EQ }
  | "<" { LT }
  | ">" { GT }
  | "NOT" { NOT }
  | "XOR" { XOR }
  | "NXOR" { NXOR }
  | "NAND" { NAND }
  | "NOR" { NOR }
  | "!&" { NAND }
  | "!|" { NOR }
  | "&" { AND }
  | "|" { OR }
  | "AND" { AND }
  | "OR" { OR }
  | "SIN" {SIN}
  | "COS" {COS}
  | "TAN" {TAN}
  | "!" { EXCL }
  | "ARCTAN" {ARCTAN}
  | "ARCCOS" {ARCCOS}
  | "ARCSIN" {ARCSIN}
  | "2x2" {TWOVAR}
  | "3x3" {THREEVAR}
  | float { NUM (float_of_string (Lexing.lexeme lexbuf)) }
  | "VAR" { VAR }
  | ":" { COLON }
  | "GOTO" { GOTO }
  | "LBL" { LBL }
  | "END" { END }
  | "IF" { IF }
  | "THEN" { THEN } 
  | "ELSE" { ELSE }
  | "true" { BOOL (bool_of_string (Lexing.lexeme lexbuf)) }
  | "false" { BOOL (bool_of_string (Lexing.lexeme lexbuf)) }
  | "FUNC" {FUNC}
  | "MATRIX" {MATRIX}
  | endchar { END }
  | word { NAME (Lexing.lexeme lexbuf) }
  | eof { EOF }