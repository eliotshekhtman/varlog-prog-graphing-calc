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
  | Subt
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

type value = 
  | Num of float 
  | Bool of bool 

type expr = 
  | Keyword of key * expr
  | Val of value
  | Var of string 
  | Binop of bop * expr * expr
  | Derivative of der * expr * expr
  | Ternop of top * (expr * expr) * expr
  | Uniop of uop * expr
  | Bind of string * expr * expr
  (* | Let of string * expr * expr
     | If of expr * expr * expr *)

