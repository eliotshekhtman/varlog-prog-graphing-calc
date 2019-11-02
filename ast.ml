type bop = 
  | Add
  | Mult 
  | Leq
  | Subt
  | Div 
  | Pow

type expr = 
  | Var of string 
  | Num of float
  | Bool of bool 
  | Binop of bop * expr * expr
  | Let of string * expr * expr
  | If of expr * expr * expr