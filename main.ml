open Ast
open Graphing

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
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(** [string_of_val e] converts [e] to a string.
    Requires: [e] is a value. *)
let string_of_val (e : expr) : string =
  match e with
  | Val (Num i) -> string_of_float i
  | Val (Bool b) -> string_of_bool b
  | _ -> failwith "precondition violated: not a value"


(** [is_value e] is whether [e] is a value. *)
let is_value_graph : expr -> bool = function
  | Val _ -> true
  | Var "x" -> true
  | _ -> false

(** [get_value v] is the value contained within value constructor [v]*)
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
    let area = acc +. 0.0001 *. fn (a+.0.0001) in
    integrate (a+.0.0001) b area fn 

let derive a fn = 
  (fn (a+.0.0001) -. fn (a-.0.0001)) /. 0.0002

(** [step e] takes a single step of evaluation of [e]. *)
let rec step : expr -> expr = function
  | Keyword _ -> failwith "precondition violated: too many keywords"
  | Val _ -> failwith "Does not step"
  | Var _ -> failwith "precondition violated: variable"
  | Ternop (top, (e1,e2), e3) when is_value e1 && is_value e2 ->
    step_top top e1 e2 e3
  | Ternop (top, (e1,e2), e3) when is_value e1 -> Ternop(top, (e1, step e2), e3)
  | Ternop (top, (e1,e2), e3)-> Ternop(top, (step e1, e2), e3)
  | Binop (bop, e1, e2) when is_value e1 && is_value e2 -> 
    step_bop bop e1 e2
  | Binop (bop, e1, e2) when is_value e1 ->
    Binop (bop, e1, step e2)
  | Binop (bop, e1, e2) -> Binop (bop, step e1, e2)
  | Uniop (uop, e) when is_value e ->
    step_uop uop e
  | Uniop (uop, e) -> Uniop (uop, step e)  
  | Derivative (der,e1,e2) when is_value e1 -> step_deriv der e1 e2
  | Derivative (der,e1,e2) -> Derivative (der, step e1, e2)
  |_-> failwith "Ya fucked up"

and step_deriv der e1 e2 = match der, e1, e2 with
  | Der, Val (Num a), b -> Val (Num (derive a (b|> eval_graph)))
  |_-> failwith "you are dumb"

and step_top top e1 e2 e3 = match top, e1, e2, e3 with
  | Integral, Val (Num a), Val (Num b), c -> Val (Num (integrate a b 0. (c|>eval_graph)))
  |_-> failwith "precondition violated"

(** [step_bop bop v1 v2] implements the primitive operation
    [v1 bop v2].  Requires: [v1] and [v2] are both values. *)
and step_bop bop e1 e2 = match bop, e1, e2 with
  | Add, Val (Num a), Val (Num b) -> Val (Num (a +. b))
  | Mult, Val (Num a), Val (Num b) -> Val (Num (a *. b))
  | Subt, Val (Num a), Val (Num b) -> Val (Num (a -. b))
  | Div, Val (Num a), Val (Num b) -> Val (Num (a /. b))
  | Pow, Val (Num a), Val (Num b) -> Val (Num (a ** b))
  | Perm, Val (Num a), Val (Num b) -> 
    let res = (fact a) /. ((fact b) *. (a -. b |> fact)) in Val (Num (res))
  | Comb, Val (Num a), Val (Num b) -> Val (Num ((fact a) /. (fact b)))
  (* | Der, Num a, b -> Num (derive a (b |> eval_graph)) *)
  | _ -> failwith "precondition violated: bop"

(** [step_uop uop v] implements the primitive operation
    [uop v].  Requires: [v] is a value. *)
and step_uop uop e = match uop, e with
  | Subt, Val (Num a) -> Val (Num (~-.a))
  | Fact, Val (Num a) -> Val (Num (fact a))
  | Sin, Val (Num a) -> Val (Num (sin a))
  | Cos, Val (Num a) -> Val (Num (cos a))
  | Tan, Val (Num a) -> Val (Num (tan a))
  | ArcSin, Val (Num a) -> Val (Num (asin a))
  | ArcCos,Val (Num a) -> Val (Num (acos a))
  | ArcTan, Val (Num a) -> Val (Num (atan a))
  | _ -> failwith "precondition violated: uop"


(** [eval e] fully evaluates [e] step-wise to a value. *)
let rec eval (e : expr) : expr = 
  if is_value e then e
  else e |> step |> eval


(** [interp s] interprets [s] by lexing and parsing it, 
    evaluating it, and returning either the value in the case of an expression
    input or an indication that the expression has been graphed in the case
    of a graphing request.*)
let interp (s : string) : string =
  match s |> parse with 
  | Keyword (k, e) -> begin  
      match k with
      | Eval -> e |> eval |> string_of_val
      | Graph ->
        let x_min = print_string "SET MIN X> "; read_line () |> float_of_string in
        let x_max = print_string "SET MAX X> "; read_line () |> float_of_string in
        let y_min = print_string "SET MIN Y> "; read_line () |> float_of_string in
        let y_max = print_string "SET MAX Y> "; read_line () |> float_of_string in
        graph_func x_min x_max y_min y_max 0 (e |> eval_graph); "Graphed"
    end
  | _ -> failwith "no keyword"