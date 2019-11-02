open Ast

let rec fact n = if n = 0. then 1. else n *. fact (n -. 1.)

(** [is_value e] is whether [e] is a value. *)
let is_value : expr -> bool = function
  | Num _ -> true
  | _ -> false

(** [step e] takes a single step of evaluation of [e]. *)
let rec step : expr -> expr = function
  | Num _ -> failwith "Does not step"
  | Binop (bop, e1, e2) when is_value e1 && is_value e2 -> 
    step_bop bop e1 e2
  | Binop (bop, e1, e2) when is_value e1 ->
    Binop (bop, e1, step e2)
  | Binop (bop, e1, e2) -> Binop (bop, step e1, e2)
  | Uniop (uop, e) when is_value e ->
    step_uop uop e
  | Uniop (uop, e) -> Uniop (uop, step e)

(** [step_bop bop v1 v2] implements the primitive operation
    [v1 bop v2].  Requires: [v1] and [v2] are both values. *)
and step_bop bop e1 e2 = match bop, e1, e2 with
  | Add, Num a, Num b -> Num (a +. b)
  | Mult, Num a, Num b -> Num (a *. b)
  | Subt, Num a, Num b -> Num (a -. b)
  | Div, Num a, Num b -> Num (a /. b)
  | Pow, Num a, Num b -> Num (a ** b)
  | _ -> failwith "precondition violated"

and step_uop uop e = match uop, e with
  | Fact, Num a -> Num (fact a)


(** [eval e] fully evaluates [e] to a value. *)
let rec eval (e : expr) : expr = 
  if is_value e then e
  else e |> step |> eval

(** [parse s] parses [s] into an AST. *)
let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(** [string_of_val e] converts [e] to a string.
    Requires: [e] is a value. *)
let string_of_val (e : expr) : string =
  match e with
  | Num i -> string_of_float i
  | Binop _ -> failwith "precondition violated"

(** [interp s] interprets [s] by lexing and parsing it, 
    evaluating it, and converting the result to a string. *)
let interp (s : string) : string =
  s |> parse |> eval |> string_of_val
