open Ast 

let rec fact n = 
  if n < 0. then failwith "not integer"
  else if n = 0. then 1. else n *. fact (n -. 1.)

(** [is_value e] is whether [e] is a value. *)
let is_value : expr -> bool = function
  | Val _ -> true
  | Var _ -> failwith "precondition violated: variable"
  | _ -> false


(** [parse s] parses [s] into an AST. *)
let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.parse_expr Lexer.read lexbuf in
  ast

let is_int f = 
  let f' = f |> int_of_float |> float_of_int in 
  if f = f' then true 
  else false

(** [string_of_val e] converts [e] to a string.
    Requires: [e] is a value. *)
let string_of_val (v : value) : string =
  match v with
  | Num i -> 
    if is_int i then i |> int_of_float |> string_of_int
    else string_of_float i
  | Bool b -> string_of_bool b
  | Str s -> s


(** [is_value e] is whether [e] is a value. *)
let is_value_graph : expr -> bool = function
  | Val _ -> true
  | Var "x" -> true
  | _ -> false

let get_val v = function
  | Val (Num n) -> n
  | Var "x" -> v 
  | _ -> failwith "precondition violated"

(** [step e] takes a single step in the graphing of [e]*)
let rec step_graph v = function
  | Keyword _ -> failwith "precondition violated: too many keywords"
  | Val _ -> failwith "Does not step"
  | Var _ -> failwith "precondition violated: variable"
  (* | Ternop (top, (e1,e2), e3) when is_value e1 && is_value e2 ->
     step_top top e1 e2 e3
     | Ternop (top, (e1,e2), e3) when is_value e1 -> Ternop(top, (e1, step e2), e3)
     | Ternop (top, (e1,e2), e3)-> Ternop(top, (step e1, e2), e3) *)
  | Binop (bop, e1, e2) when is_value_graph e1 && is_value_graph e2 -> 
    step_bop v bop e1 e2
  | Binop (bop, e1, e2) when is_value_graph e1 ->
    Binop (bop, e1, step_graph v e2)
  | Binop (bop, e1, e2) -> Binop (bop, step_graph v e1, e2)
  | Uniop (uop, e) when is_value_graph e ->
    step_uop v uop e
  | Uniop (uop, e) -> Uniop (uop, step_graph v e)
  | _ -> failwith "unimplemented"

(** [step_bop bop v1 v2] implements the primitive operation
    [v1 bop v2].  Requires: [v1] and [v2] are both values. *)
and step_bop v bop e1 e2 = match bop with
  | Add -> Val (Num ((get_val v e1) +. (get_val v e2)))
  | Mult -> Val (Num ((get_val v e1) *. (get_val v e2)))
  | Subt -> Val (Num ((get_val v e1) -. (get_val v e2)))
  | Div -> Val (Num ((get_val v e1) /. (get_val v e2)))
  | Pow -> Val (Num ((get_val v e1) ** (get_val v e2)))
  | _ -> failwith "precondition violated: bop"

(** [step_bop uop v] implements the primitive operation
    [uop v].  Requires: [v] is a value. *)
and step_uop v uop e = match uop with
  | Fact -> Val (Num (fact (get_val v e)))
  | Sin -> Val (Num (sin (get_val v e)))
  | Cos -> Val (Num (cos (get_val v e)))
  | Tan -> Val (Num (tan (get_val v e)))
  | ArcTan -> Val (Num (atan (get_val v e)))
  | ArcCos -> Val (Num (acos (get_val v e)))
  | ArcSin -> Val (Num (asin (get_val v e)))
  | _ -> failwith "precondition violated: uop"


(** [eval e] fully graphs [e].*)
let rec eval_graph (e : expr) (v : float) : float = 
  match e with
  | Val (Num n) -> n
  | Var "x" -> v
  | _ -> eval_graph (e |> step_graph v) v


let rec integrate a b acc fn = 
  if(a >= b) then acc 
  else
    let area = acc +. 0.000001 *. fn (a+.0.000001) in
    integrate (a+.0.000001) b area fn 

let derive a fn = 
  (fn (a+.0.000000001) -. fn (a-.0.000000001)) /. 0.000000002

let add_helper v1 v2 = 
  match v1, v2 with 
  | Num a, Num b -> Num (a +. b)
  | Str a, Str b -> Str (a ^ b)
  | Num a, Str b -> Str ((v1 |> string_of_val) ^ b)
  | Str a, Num b -> Str (a ^ (v2 |> string_of_val))
  | _ -> failwith "precondition violated: add types"

let eq_helper v1 v2 = 
  match v1, v2 with 
  | Num a, Num b -> Bool (a = b)
  | Str a, Str b -> Bool (a = b)
  | Bool a, Bool b -> Bool (a = b) 
  | _ -> Bool false

let lt_helper v1 v2 = 
  match v1, v2 with 
  | Num a, Num b -> Bool (a < b)
  | Str a, Str b -> Bool (a < b)
  | Bool a, Bool b -> Bool (a < b) 
  | _ -> Bool false

let gt_helper v1 v2 = 
  match v1, v2 with 
  | Num a, Num b -> Bool (a > b)
  | Str a, Str b -> Bool (a > b)
  | Bool a, Bool b -> Bool (a > b) 
  | _ -> Bool false

let not_val v = 
  match v with 
  | Bool b -> Bool (not b)
  | _ -> failwith "precondition violated: not bool"

let substitute vl s = 
  try List.assoc s vl with _ -> failwith "precondition violated: unbound var"

let pull_num = function 
  | Num n -> n 
  | _ -> failwith "precondition violated: not a number"

let rec eval_expr vl e = 
  match e with 
  | Keyword _ -> failwith "precondition violated: too many keywords"
  | Val v -> (v, vl) 
  | PreString s -> 
    let len = String.length s - 2 in 
    ((Str (String.sub s 1 len)), vl)
  | Var v -> ((substitute vl v), vl) 
  | Uniop (uop, e) -> eval_uop vl uop e
  | Binop (bop, e1, e2) -> eval_bop vl bop e1 e2 
  | Ternop (top, (e1, e2), e3) -> eval_top vl top e1 e2 e3
  | Derivative (der, e1, e2) -> eval_deriv vl der e1 e2
  | _ -> failwith "lol right"
and eval_bop vl b e1 e2 = 
  let r1 = eval_expr vl e1 in 
  let (r2, vl2) = eval_expr (snd r1) e2 in 
  match b, (fst r1), r2 with 
  | Add, v1, v2 -> (add_helper v1 v2, vl2)
  | Subt, Num a, Num b -> (Num (a -. b), vl2)
  | Mult, Num a, Num b -> (Num (a *. b), vl2)
  | Div, Num a, Num b -> (Num (a /. b), vl2)
  | Pow, Num a, Num b -> (Num (a ** b), vl2)
  | Perm, Num a, Num b -> 
    let res = (fact a) /. ((fact b) *. (a -. b |> fact)) in (Num (res), vl2)
  | Comb, Num a, Num b -> (Num ((fact a) /. (fact b)), vl2)
  | Eq, v1, v2 -> (eq_helper v1 v2, vl2)
  | Neq, v1, v2 -> (eq_helper v1 v2 |> not_val, vl2)
  | Lt, v1, v2 -> (lt_helper v1 v2, vl2)
  | Geq, v1, v2 -> (lt_helper v1 v2 |> not_val, vl2)
  | Gt, v1, v2 -> (gt_helper v1 v2, vl2)
  | Leq, v1, v2 -> (gt_helper v1 v2 |> not_val, vl2)
  | _ -> failwith "precondition violated: bop input types"
and eval_uop vl u e = 
  let (r, vl') = eval_expr vl e in 
  match u, r with 
  | Subt, Num n -> (Num (~-.n), vl')
  | Fact, Num n -> (Num (fact n), vl')
  | Sin, Num n -> (Num (sin n), vl')
  | Cos, Num n -> (Num (cos n), vl')
  | Tan, Num n -> (Num (tan n), vl')
  | ArcSin, Num n -> (Num (asin n), vl')
  | ArcCos, Num n -> (Num (acos n), vl')
  | ArcTan, Num n -> (Num (atan n), vl')
  | _ -> failwith "precondition violated: uop"
and eval_top vl top e1 e2 e3 = 
  let r1 = eval_expr vl e1 in 
  let r2 = eval_expr (snd r1) e2 in 
  match top, (fst r1), (fst r2) with 
  | Integral, Num a, Num b -> (Num (integrate a b 0. (e3 |> eval_graph)), (snd r2))
  | _ -> failwith "precondition violated: top"
and eval_deriv vl der e1 e2 = 
  let r1 = eval_expr vl e1 in 
  match der, (fst r1) with 
  | Der, Num a -> (Num (derive a (e2 |> eval_graph)), (snd r1))
  | _ -> failwith "precondition violated: derivative"