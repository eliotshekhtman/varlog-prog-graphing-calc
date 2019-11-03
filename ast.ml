type bop = 
  | Add
  | Mult 
  | Leq
  | Subt
  | Div 
  | Pow

type uop = 
  | Fact

type expr = 
  | Eval of expr
  (* | Var of string  *)
  | Num of float
  (* | Bool of bool  *)
  | Binop of bop * expr * expr
  | Uniop of uop * expr
  (* | Let of string * expr * expr
     | If of expr * expr * expr *)

