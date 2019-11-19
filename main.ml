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
  let ast = Parser.parse_expr Lexer.read lexbuf in
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

let close_int (f : float) = 
  let f' = f |> int_of_float |> float_of_int in 
  if f > 0. then
    if f < f' +. 0.0000001 && f > f' -. 0.0000001 then (true, f') 
    else 
    if f < f' +. 1.0000001 && f > f' +. 1. -. 0.0000001 then (true, f' +. 1.)
    else (false, f)
  else 
  if f < f' +. 0.0000001 && f > f' -. 0.0000001 then (true, f') 
  else 
  if f > f' -. 1.0000001 && f < f' -. 1. +. 0.0000001 then (true, f' -. 1.)
  else (false, f) 

let rec newton_disp n lst acc = 
  match acc, lst with 
  | a, _ when a > n -> print_endline ("\n" ^ "Input coefficients: ")
  | a, h :: t when a = n -> begin 
      print_string (h ^ "x^" ^ (a |> string_of_int)); 
      newton_disp n t (a+1)
    end
  | a, h :: t -> begin 
      print_string (h ^ "x^" ^ (a |> string_of_int) ^ " + "); 
      newton_disp n t (a+1)
    end
  | _, _ -> failwith "precondition violated: power"

let rec newton_inputs n lst = 
  match n, lst with 
  | n, _ when n < 0 -> []
  | n, h :: t -> begin 
      print_string (h ^ "> ");
      let value = (read_line () |> parse |> eval |> get_val 0.) in 
      value :: newton_inputs (n-1) t
    end
  | _, _ -> failwith "aaa"

let rec newton_make lst x = 
  let rec deriv lst x = 
    match lst with 
    | [] -> 0.
    | h :: [] -> 0.
    | h :: t -> 
      let ele = (t |> List.length |> float_of_int) in 
      ele *. h *. x ** (ele -. 1.) +. deriv t x
  in 
  let rec norm lst x = 
    match lst with 
    | [] -> 0.
    | h :: t -> 
      let ele = (t |> List.length |> float_of_int) in 
      h *. x ** ele +. norm t x
  in 
  let numer = norm lst x in 
  let denom = deriv lst x in 
  if denom = 0. || numer = 0. || denom = nan || numer = nan then x 
  else x -. (numer /. denom)

let rec newton_apply f = 
  let rec apply n f r = 
    let res = f r in
    if r = res then r 
    else
    if n <= 0 then r else apply (n-1) f res
  in
  print_string "x = ";
  match read_line () with 
  | "QUIT" -> "Quitting" 
  | s -> 
    let inp = (s |> parse |> eval |> get_val 0.) in 
    let res = apply 1000000 f inp in 
    let res' = close_int res in
    res |> string_of_float |> print_endline; 
    (if fst res' then 
       print_endline ("Similar integer: " ^ (snd res' |> string_of_float)) 
     else ());
    newton_apply f

let newton_helper n = 
  if n < 0. || n > 5. then failwith "Error: Keep power to within 0-5"
  else 
    let lst = ["A"; "B"; "C"; "D"; "E"; "F"] in 
    let n' = int_of_float n in 
    newton_disp n' lst 0; 
    let inputs = newton_inputs n' lst in 
    let f = inputs |> List.rev |> newton_make in 
    newton_apply f


let matrix = [| [|1.;2.;3.|];[|0.;1.;5.|];[|5.;6.;0.|] |]
let matrix_made = Array.make_matrix 3 3 4

let find_det a d b c = a *. d -. b *. c

let determined (matrix:float array array ) = 
  let a1 = matrix.(1).(1) in
  let d1 = matrix.(2).(2) in
  let b1 = matrix.(1).(2) in
  let c1 = matrix.(2).(1) in
  let det1 = find_det a1 d1 b1 c1 in 
  print_endline (string_of_float det1);
  let a2 = matrix.(1).(0) in
  let d2 = matrix.(2).(2) in
  let b2 = matrix.(1).(2) in
  let c2 = matrix.(2).(0) in
  let det2 = -1. *. find_det a2 d2 b2 c2 in
  print_endline (string_of_float det2);
  let a3 = matrix.(1).(0) in
  let d3 = matrix.(2).(1) in
  let b3 = matrix.(1).(1) in
  let c3 = matrix.(2).(0) in
  let det3 = find_det a3 d3 b3 c3 in
  print_endline (string_of_float det3);
  let a4 = matrix.(0).(1) in
  let d4 = matrix.(2).(2) in
  let b4 = matrix.(0).(2) in
  let c4 = matrix.(2).(1) in
  let det4 = -1. *.find_det a4 d4 b4 c4 in
  let a5 = matrix.(0).(0) in
  let d5 = matrix.(2).(2) in
  let b5 = matrix.(0).(2) in
  let c5 = matrix.(2).(0) in
  let det5 = find_det a5 d5 b5 c5 in
  let a6 = matrix.(0).(0) in
  let d6 = matrix.(2).(1) in
  let b6 = matrix.(0).(1) in
  let c6 = matrix.(2).(0) in
  let det6 = -1. *.find_det a6 d6 b6 c6 in
  let a7 = matrix.(0).(1) in
  let d7 = matrix.(1).(2) in
  let b7 = matrix.(0).(2) in
  let c7 = matrix.(1).(1) in
  let det7 = find_det a7 d7 b7 c7 in
  let a8 = matrix.(0).(0) in
  let d8 = matrix.(1).(2) in
  let b8 = matrix.(0).(2) in
  let c8 = matrix.(1).(0) in
  let det8 = -1. *.find_det a8 d8 b8 c8 in
  let a9 = matrix.(0).(0) in
  let d9 = matrix.(1).(1) in
  let b9 = matrix.(0).(1) in
  let c9 = matrix.(1).(0) in
  let det9 = find_det a9 d9 b9 c9 in
  let det = (matrix.(0).(0)*.det1) +. (matrix.(0).(1) *. det2) +. (matrix.(0).(2)*.det3) in
  print_endline (string_of_float det);
  let new_matrix = ([|[|det1;det2;det3;|]; [|det4;det5;det6;|];[|det7;det8;det9;|]|], det) in
  new_matrix

let reflect (matrix: float array array) =
  let temp1 = matrix.(1).(0) in
  matrix.(1).(0) <- matrix.(0).(1);
  matrix.(0).(1) <- temp1; 
  let temp2 = matrix.(2).(0) in
  matrix.(2).(0) <- matrix.(0).(2);
  matrix.(0).(2) <- temp2; 
  let temp3 = matrix.(2).(1) in
  matrix.(2).(1) <- matrix.(1).(2);
  matrix.(1).(2) <- temp3; 
  matrix

let normalized matrix det = 
  let inverse_det = det ** -1. in
  matrix.(0).(0) <- inverse_det *. matrix.(0).(0);
  matrix.(0).(1) <- inverse_det *. matrix.(0).(1);
  matrix.(0).(2) <- inverse_det *. matrix.(0).(2);
  matrix.(1).(0) <- inverse_det *. matrix.(1).(0);
  matrix.(1).(1) <- inverse_det *. matrix.(1).(1);
  matrix.(1).(2) <- inverse_det *. matrix.(1).(2);
  matrix.(2).(0) <- inverse_det *. matrix.(2).(0);
  matrix.(2).(1) <- inverse_det *. matrix.(2).(1);
  matrix.(2).(2) <- inverse_det *. matrix.(2).(2);
  matrix

let inversed (matrix: float array array) = 
  let determinant_and_matrix = determined matrix in
  let reflected = reflect (determinant_and_matrix |> fst) in
  let final = normalized reflected (determinant_and_matrix |> snd) in
  final

let matrix_mult (matrix: float array array) (vector: float array) = 
  let inverse_matrix = inversed matrix in
  let answer_vector = [|0.;0.;0.|] in
  for i = 0 to 2 do
    let acc = ref 0. in
    for j = 0 to 2 do
      acc := !acc +. inverse_matrix.(i).(j) *. vector.(j);
    done;
    answer_vector.(i) <- !acc
  done;
  answer_vector



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
      | Newton -> newton_helper (e |> eval |> get_val 0.)
    end
  | _ -> failwith "no keyword"