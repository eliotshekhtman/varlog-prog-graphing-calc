open Ast
open Graphing

let rec fact n = if n = 0. then 1. else n *. fact (n -. 1.)

(** [is_value e] is whether [e] is a value. *)
let is_value : expr -> bool = function
  | Num _ -> true
  | XVar -> failwith "precondition violated: variable"
  | _ -> false

(** [step e] takes a single step of evaluation of [e]. *)
let rec step : expr -> expr = function
  | Keyword _ -> failwith "precondition violated: too many keywords"
  | Num _ -> failwith "Does not step"
  | XVar -> failwith "precondition violated: variable"
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
  | _ -> failwith "precondition violated: bop"

(** [step_uop uop v] implements the primitive operation
    [uop v].  Requires: [v] is a value. *)
and step_uop uop e = match uop, e with
  | Fact, Num a -> Num (fact a)
  | _ -> failwith "precondition violated: uop"


(** [eval e] fully evaluates [e] step-wise to a value. *)
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
  | _ -> failwith "precondition violated: sov"


(** [is_value e] is whether [e] is a value. *)
let is_value_graph : expr -> bool = function
  | Num _ -> true
  | XVar -> true
  | _ -> false

(** [get_value v] is the value contained within value constructor [v]*)
let get_val v = function
  | Num n -> n
  | XVar -> v 
  | _ -> failwith "precondition violated"

(** [step e] takes a single step in the graphing of [e]*)
let rec step_graph v = function
  | Keyword _ -> failwith "precondition violated: too many keywords"
  | Num _ -> failwith "Does not step"
  | XVar -> failwith "precondition violated: variable"
  | Binop (bop, e1, e2) when is_value_graph e1 && is_value_graph e2 -> 
    step_bop v bop e1 e2
  | Binop (bop, e1, e2) when is_value_graph e1 ->
    Binop (bop, e1, step_graph v e2)
  | Binop (bop, e1, e2) -> Binop (bop, step_graph v e1, e2)
  | Uniop (uop, e) when is_value_graph e ->
    step_uop v uop e
  | Uniop (uop, e) -> Uniop (uop, step_graph v e)

(** [step_bop bop v1 v2] implements the primitive operation
    [v1 bop v2].  Requires: [v1] and [v2] are both values. *)
and step_bop v bop e1 e2 = match bop with
  | Add -> Num ((get_val v e1) +. (get_val v e2))
  | Mult -> Num ((get_val v e1) *. (get_val v e2))
  | Subt -> Num ((get_val v e1) -. (get_val v e2))
  | Div -> Num ((get_val v e1) /. (get_val v e2))
  | Pow -> Num ((get_val v e1) ** (get_val v e2))
  | _ -> failwith "precondition violated: bop"

(** [step_bop uop v] implements the primitive operation
    [uop v].  Requires: [v] is a value. *)
and step_uop v uop e = match uop with
  | Fact -> Num (fact (get_val v e))
  | _ -> failwith "precondition violated: uop"


(** [eval e] fully graphs [e].*)
let rec eval_graph (e : expr) (v : float) : float = 
  match e with
  | Num n -> n
  | XVar -> v
  | _ -> eval_graph (e |> step_graph v) v

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