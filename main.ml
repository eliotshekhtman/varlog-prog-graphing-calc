open Ast
open Graphing
open Evalexpr
open String
open Stdlib

exception No_keyword

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
      let value = (read_line () |> parse |> eval_expr [] |> fst |> pull_num) in 
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

let rec newton_make' e x = 
  let deriv eq n = derive n eq in
  let n = eval_graph e in 
  let d = deriv n in 
  let numer = n x in let denom = d x in 
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
    let inp = (s |> parse |> eval_expr [] |> fst |> pull_num) in 
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

let newton_helper' e = 
  let f = newton_make' e in 
  print_endline "Enter guesses for values: ";
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
  (* print_endline (string_of_float det1); *)
  let a2 = matrix.(1).(0) in
  let d2 = matrix.(2).(2) in
  let b2 = matrix.(1).(2) in
  let c2 = matrix.(2).(0) in
  let det2 = -1. *. find_det a2 d2 b2 c2 in
  (* print_endline (string_of_float det2); *)
  let a3 = matrix.(1).(0) in
  let d3 = matrix.(2).(1) in
  let b3 = matrix.(1).(1) in
  let c3 = matrix.(2).(0) in
  let det3 = find_det a3 d3 b3 c3 in
  (* print_endline (string_of_float det3); *)
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
  (* print_endline (string_of_float det); *)
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

let solve_linear_equation (matrix: float array array) (vector: float array) = 
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

let exec_helper v = 
  match v with 
  | Str s -> Main_lang.interp (s ^ ".txt")
  | _ -> "Error: invalid input: requires a string filename input"


let linear_prompt = "Please input the first equation of your system of equations. Each equation should be written
  (there is a max of 3 unknowns allowed) in this form:\n 
  (5+3), 0, (-4*1), (4/2) \n where this corresponds to the equation 8x + 0y -4z = 2\n\nequation 1>"

(* let rec rmv_empty_elements arr new_arr= 
   match arr with
   | [] -> List.rev new_arr
   | head::tail ->
    (*If the element represented by [head] is NOT "", prepend to [new_arr]*)
    if(not (head = "")) then rmv_empty_elements tail (head::new_arr)
    else rmv_empty_elements tail new_arr *)

let parse_lin_equation str = 
  (*turn the string to an array by using [String.split_on_char sep]*)
  let string_array = String.split_on_char (',') (str) in
  match string_array with
  |s1::s2::s3::s4::[] -> 
    let x = s1|>parse|> eval_expr [] |> fst |> pull_num in
    let y = s2|>parse|> eval_expr [] |> fst |> pull_num in
    let z = s3|>parse|> eval_expr [] |> fst |> pull_num in
    let answer1 = s4|> parse |> eval_expr [] |> fst |> pull_num in
    ([|x;y;z|], answer1);
  |_-> failwith "not enough values"

let print_linear_equation_answer arr =
  print_string ("x = " ^ string_of_float arr.(0)^", ");
  print_string ("y = " ^ string_of_float arr.(1)^", ");
  print_string ("z = " ^ string_of_float arr.(2));
  ()

(** [interp s] interprets [s] by lexing and parsing it, 
    evaluating it, and returning either the value in the case of an expression
    input or an indication that the expression has been graphed in the case
    of a graphing request.*)
let interp (s : string) : string =
  match s |> parse with 
  | Keyword (k, e) -> begin  
      match k with
      | Eval -> e |> eval_expr [] |> fst |> string_of_val
      | Graph ->
        let x_min = print_string "SET MIN X> "; read_line () |> float_of_string in
        let x_max = print_string "SET MAX X> "; read_line () |> float_of_string in
        let y_min = print_string "SET MIN Y> "; read_line () |> float_of_string in
        let y_max = print_string "SET MAX Y> "; read_line () |> float_of_string in
        graph_func x_min x_max y_min y_max 0 (e |> eval_graph); "Graphed"
      | Newton -> newton_helper' e
      (* newton_helper (e |> eval_expr [] |> fst |> (fun v -> match v with Num n -> n | _ -> failwith "Error: invalid input: not a number")) *)
      | Exec -> exec_helper (e |> eval_expr [] |> fst)
    end
  |Solver -> begin 
      print_string linear_prompt;
      let eq1 = read_line() |> parse_lin_equation in
      print_string "Please input equation 2 in the same format:\n\nequation 2> ";
      let eq2 = read_line() |> parse_lin_equation in
      print_string "Please input equation 3 in the same format:\n\nequation 3> ";
      let eq3 = read_line() |> parse_lin_equation in
      let matrix = [|(fst eq1); (fst eq2); (fst eq3)|] in
      let target_vector = [|(snd eq1);(snd eq2);(snd eq3)|] in
      let answer = solve_linear_equation matrix target_vector in
      print_linear_equation_answer answer;
      "\nSolved!!!!!!!!!!" 
    end
  | _ -> raise No_keyword