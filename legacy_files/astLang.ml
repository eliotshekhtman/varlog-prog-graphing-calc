type bop = 
  | Add
  | Mult 
  | Leq
  | Subt
  | Div 
  | Pow

type uop = 
  | Fact

type value = 
  | Num of float 
  | Bool of bool

type expr = 
  | Var of string 
  | Val of value
  | Binop of bop * expr * expr
  | Uniop of uop * expr
  | Bind of string * expr * expr
  | If of expr * expr * expr

