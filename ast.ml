type bop = 
  | Add
  | Mult 
  | Subt
  | Div 
  | Pow
  | Comb 
  | Perm
  | Eq 
  | Lt 
  | Gt 
  | Geq 
  | Leq 
  | Neq 

type boop = 
  | And 
  | Or 
  | Xor 

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
  | Not

type key =
  | Eval
  | Graph
  | Newton
  | Exec
  (* | Derivative *)

type value = 
  | Num of float 
  | Bool of bool 
  | Str of string

type expr = 
  | Solver of string
  | PreString of string
  | Keyword of key * expr
  | Val of value
  | Var of string 
  | Binop of bop * expr * expr
  | Boolop of boop * expr * expr
  | Derivative of der * expr * expr
  | Ternop of top * (expr * expr) * expr
  | Uniop of uop * expr
  | Bind of string * expr * expr
  | Disp of expr * expr
  (* | Let of string * expr * expr
     | If of expr * expr * expr *)

type defn = 
  | DBind of string * expr * defn 
  | DAssign of string * expr * defn
  | DIf of expr * defn * defn * defn
  | DDisp of expr * defn 
  | DGoto of string * defn
  | DLabel of string * defn
  | DEnd 