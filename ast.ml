exception SyntaxError

(**type id represents an identifier linked to a value in a VarLog*)
type id = string

(**type bop represents a binary operation*)
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

(**type boop represents a binary operation*)
type boop = 
  | And 
  | Or 
  | Xor

(**type der represents a derivative*)
type der = 
  | Der

(**type top represents a ternary operation*)
type top =
  | Integral

(**type uop represents a unary operation*)
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

(**type color represents the color of the graph*)
type color = 
  | Red
  | Blue 
  | Green 
  | Yellow 
  | Black

(**type expr represents an expression*)
type expr = 
  | MakeMatrix of expr * expr
  | MakeVarMat of expr * expr
  | MatrixGet of expr * expr * expr
  | PreString of string
  | Val of value
  | Var of string 
  | Binop of bop * expr * expr
  | Boolop of boop * expr * expr
  | Derivative of der * expr * expr
  | Ternop of top * (expr * expr) * expr
  | Uniop of uop * expr
  | GetKey
  | Prompt
  | RandInt of expr * expr
  | StructGet of string * string
  | ObjectGet of string * string
  | Application of id * expr list
  | ComplApp of expr * expr list
  | Ternary of expr * expr * expr
  (**type value represents a value*)
and value = 
  | Num of float 
  | Bool of bool 
  | Str of string
  | Closure of id list * defn * ((string * value) list)
  | Matrix of float array array
  | VarMat of value array array
  | Built of (string * value) list
  | Struct of id list * defn
  | Class of id list * defn 
  | Object of (string * value) list
  | Color of color
  | Null
  (**type defn represents a definition*)
and defn = 
  | DPrompt of string * defn
  | DBind of string * expr * defn 
  | DAssign of string * expr * defn
  | DIf of expr * defn * defn * defn
  | DDisp of expr * defn 
  | DGoto of string * defn
  | DGotoSub of string * defn
  | DLabel of string * defn
  | DOutput of expr * expr * expr * expr * defn
  | DMatrixSet of expr * expr * expr * expr * defn
  | DGraph of expr * defn
  | DLine of expr * expr * expr * expr * defn
  | DReturn of expr * defn
  | DFunction of id * id list * defn * defn
  | DDefStruct of id * id list * defn * defn
  | DDefClass of id * id list * defn * defn
  | DStructSet of string * string * expr * defn
  | DObjSet of string * string * expr * defn
  | DWhile of expr * defn * defn
  | DEnd 
  (**type phrase represents a phrase containing either an expression or a
     definition*)
type phrase = 
  | Expr of expr 
  | Defn of defn
  | Graph of expr
  | Newton of expr
  | Exec of expr
  | Solver of string
  | SetScale
