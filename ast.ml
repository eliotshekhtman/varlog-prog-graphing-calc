type bop = 
  | Add
  | Mult 
  | Leq
  | Subt
  | Div 
  | Pow
  | Comb 
  | Perm




type der = 
  | Der

type top =
  | Integral

type uop = 
  | Fact
  | Sin
  | Cos 
  | Tan 
  | ArcSin
  | ArcCos
  | ArcTan

type key =
  | Eval
  | Graph
  (* | Derivative *)

type expr = 
  | Keyword of key * expr
  | Var of string 
  | Num of float
  (* | Bool of bool  *)
  | Binop of bop * expr * expr
  | Derivative of der * expr * expr
  | Ternop of top * (expr * expr) * expr
  | Uniop of uop * expr
  | Bind of string * expr * expr
  (* | Let of string * expr * expr
     | If of expr * expr * expr *)

