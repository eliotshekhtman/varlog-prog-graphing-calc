exception SyntaxError


type id = string
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


type expr = 
  | SetScale
  | MakeMatrix of expr * expr
  | MatrixGet of expr * expr * expr
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
  | GetKey
  | Prompt
  | RandInt of expr * expr
and value = 
  | Num of float 
  | Bool of bool 
  | Str of string
  | Closure of id list * defn * ((string * value) list)
  | Matrix of float array array
  | Null
  (* | Let of string * expr * expr
     | If of expr * expr * expr *)
and defn = 
  | DPrompt of string * defn
  | DBind of string * expr * defn 
  | DAssign of string * expr * defn
  | DIf of expr * defn * defn * defn
  | DDisp of expr * defn 
  | DGoto of string * defn
  | DGotoSub of string * defn
  | DLabel of string * defn
  | DOutput of expr * expr * expr * defn
  | DMatrixSet of expr * expr * expr * expr * defn
  | DGraph of expr * defn
  | DLine of expr * expr * expr * expr * defn
  | DReturn of expr * defn
  | DFunction of id * id list * defn
  | DEnd 

type phrase = 
  | Expr of expr 
  | Defn of defn
