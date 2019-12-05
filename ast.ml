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

type expr = 
  | MakeMatrix of expr * expr
  | MatrixGet of expr * expr * expr
  | PreString of string
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
  | InstantiateStruct of string * expr list
  | StructGet of string * string
  | Application of id * expr list
and value = 
  | Num of float 
  | Bool of bool 
  | Str of string
  | Closure of id list * defn * ((string * value) list)
  | Matrix of float array array
  | Built of (string * value) list
  | Struct of id list * defn
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
  | DDefStruct of id * id list * defn * defn
  | DInstantiateStruct of string * string * expr list * defn
  | DStructSet of string * string * expr * defn
  | DEnd 

type phrase = 
  | Expr of expr 
  | Defn of defn
  | Eval of expr
  | Graph of expr
  | Newton of expr
  | Exec of expr
  | Solver of string
  | SetScale
