type bop = 
  | Add
  | Mult 
  | Leq
  | Subt
  | Div 
  | Pow

type top =
  | Integral

type uop = 
  | Fact

type key =
  | Eval
  | Graph
  (* | Derivative *)

type expr = 
  | Keyword of key * expr
  (* | Var of string  *)
  | Num of float
  | XVar
  (* | Bool of bool  *)
  | Binop of bop * expr * expr
  | Ternop of top * (expr * expr) * expr
  | Uniop of uop * expr
  (* | Let of string * expr * expr
     | If of expr * expr * expr *)

