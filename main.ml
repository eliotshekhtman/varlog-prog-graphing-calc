open Ast
open Graphing
open Eval
open String
open Stdlib
open VarLog 

(** [No_keyword] *)
exception No_keyword
exception DeterminantZero

(** [parse s] parses [s] into an AST. *)
let parse_phrase (s : string) : phrase =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.parse_phrase Lexer.read lexbuf in
  ast

(*BISECT-IGNORE-BEGIN*)

(** [close_int f] is the nearest integer, as a float, to [f] if [f] is
    less than 10^(-7) away from an integer; else, it's just [f] *)
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

(** [newton_make e x] is the result of applying [x] to one step of
    Newton's Method *)
let rec newton_make e x = 
  let deriv eq n = derive n eq in
  let n = eval_graph e in 
  let d = deriv n in 
  let numer = n x in let denom = d x in 
  if denom = 0. || numer = 0. || denom = nan || numer = nan then x 
  else x -. (numer /. denom)

(** [newton_apply f] is the string "Quitting", and prints the repl
    loop for applying different guesses to Newton's Method for 
    function [f] *)
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
    let inp = (s |> parse |> eval_expr (VarLog.empty ()) |> fst |> pull_num) in 
    let res = apply 1000000 f inp in 
    let res' = close_int res in
    res |> string_of_float |> print_endline; 
    (if fst res' then 
       print_endline ("Similar integer: " ^ (snd res' |> string_of_float)) 
     else ());
    newton_apply f

(** [newton_helper e] is "Quitting", and instantiates the repl, 
    combining the creation of [f] for [newton_apply f] *)
let newton_helper e = 
  let f = newton_make e in 
  print_endline "Enter guesses for values: ";
  newton_apply f

(**[det a d b c] is the determinant of a 2 by 2 matrix represented by
   [[a,b], [c,d]]*)
let det a d b c = a *. d -. b *. c

(**[determined_matrix2 matrix] is the determinant of a 2 by 2 matrix [matrix]*)
let determined_matrix2 matrix = 
  let a1 = matrix.(0).(0) in
  let d1 = matrix.(1).(1) in
  let b1 = matrix.(0).(1) in
  let c1 = matrix.(1).(0) in
  let det = det a1 d1 b1 c1 in
  let new_matrix = ([|
      [|d1; -1. *. b1|];
      [|-1.*.c1; a1|]
    |], det) in new_matrix

(**[make_matrix_from_det det1 det2 det3 det4 det5 det6 det7 det8 det9 det] is
   the 3 by 3 matrix of determinants of every 2 by 2 matrix in the original 
   3 by 3 matrix, tupled with the overall determinant of the 3 by 3 matrix
   matrix. The final type is (float array array * float) 
    Requires: 
      [det1] ... [det9] are the correct determinants of each 2 by 2 in the 
      larger 3 by 3 matrix
      [det] is the correct overall determinant of the 3 by 3 matrix
    Exception:
      raises [DeterminantZero] if [det] is zero*)
let make_matrix_from_det det1 det2 det3 det4 det5 det6 det7 det8 det9 det =
  if det = 0. then raise DeterminantZero
  else 
    let new_matrix = ([|[|det1;det2;det3;|]; [|det4;det5;det6;|];
                        [|det7;det8;det9;|]|], det) 
    in new_matrix

(**[determined_matrix3 matrix] is the determinant of a 3 by 3 matrix [matrix]*)
let determined_matrix3 (matrix:float array array ) = 
  let det1 = det matrix.(1).(1) matrix.(2).(2) matrix.(1).(2) matrix.(2).(1) in
  let det2 = -1. *. det matrix.(1).(0) matrix.(2).(2) matrix.(1).(2)
                matrix.(2).(0) in
  let det3 = det matrix.(1).(0) matrix.(2).(1) matrix.(1).(1) matrix.(2).(0) in
  let det4 = -1. *.det matrix.(0).(1) matrix.(2).(2) matrix.(0).(2)
                matrix.(2).(1) in
  let det5 = det matrix.(0).(0) matrix.(2).(2)  matrix.(0).(2) matrix.(2).(0) in
  let det6 = -1. *.det matrix.(0).(0) matrix.(2).(1) matrix.(0).(1)
                matrix.(2).(0) in
  let det7 = det matrix.(0).(1) matrix.(1).(2) matrix.(0).(2) matrix.(1).(1) in
  let det8 = -1. *.det matrix.(0).(0) matrix.(1).(2) matrix.(0).(2) 
                matrix.(1).(0) in
  let det9 = det matrix.(0).(0) matrix.(1).(1) matrix.(0).(1) matrix.(1).(0) in
  let det = 
    (matrix.(0).(0)*.det1) +. (matrix.(0).(1) *. det2) +. (matrix.(0).(2)*.det3)
  in make_matrix_from_det det1 det2 det3 det4 det5 det6 det7 det8 det9 det


(**[reflect matrix] is the reflection of a 3 by 3 matrix [matrix]*)
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

(**[normalized matrix det num] is the normalized [num] by [num] matrix [matrix]
   that has determinant [det]*)
let normalized matrix det num = 
  let inverse_det = if det = 0. then raise DeterminantZero else det ** -1. in
  if(num = 3) then (
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
  ) 
  else (
    matrix.(0).(0) <- inverse_det *. matrix.(0).(0);
    matrix.(0).(1) <- inverse_det *. matrix.(0).(1);
    matrix.(1).(0) <- inverse_det *. matrix.(1).(0);
    matrix.(1).(1) <- inverse_det *. matrix.(1).(1);
    matrix
  )

(**[inversed matrix num] is the inverse of a [num] by [num] matrix [matrix]*)
let inversed (matrix: float array array) num = 
  if num = 3 then 
    let determined3_matrix = determined_matrix3 matrix in
    let reflected = reflect (determined3_matrix |> fst) in
    let final = normalized reflected (determined3_matrix |> snd) num in
    final
  else 
    let determined2_matrix = determined_matrix2 matrix in
    let final = 
      normalized (determined2_matrix |> fst) (determined2_matrix |> snd) num in
    final 

(**[solve_linear_equation matrix vector num] is the solution of the set of
   linear equations with [num] unknowns, where the coefficients of the unknowns
   are represented by the [num] by [num] matrix [matrix] and the corresponding
   solutions to the equations are represented by [vector], which has length
   [num]
   Requires: [num] is either 2 or 3*)
let solve_linear_equation 
    (matrix: float array array) (vector: float array) num = 
  let inverse_matrix = inversed matrix num in
  let answer_vector = if num = 3 then [|0.;0.;0.|] else [|0.;0.|] in
  for i = 0 to num-1 do
    let acc = ref 0. in
    for j = 0 to num-1 do
      acc := !acc +. inverse_matrix.(i).(j) *. vector.(j);
    done;
    answer_vector.(i) <- !acc
  done;
  answer_vector

(**[exec_helper v] handles the execution of a .vl file represented by
   value [v]. Exceptions: Raises an exception if [v] is not a valid string
   representation*)
let exec_helper v = 
  match v with 
  | Str s -> Main_lang.interp (s ^ ".vl")
  | _ -> "Error: invalid input: requires a string filename input"


let linear_prompt = "Please input the first equation of your system of " ^ 
                    "equations. Each equation should be written " ^ 
                    "(there is a max of 3 unknowns allowed) in this form:\n " ^
                    "(5+3), 0, (-4*1), (4/2) \n where this corresponds to the" ^ 
                    " equation 8x + 0y -4z = 2\n\nequation 1>"

(**[parse_lin_equation3 str] converts a string representation of a linear
   equation with 3 unknowns, [str], into the required arrays for solving*)
let parse_lin_equation3 str  = 
  let string_array = String.split_on_char (',') (str) in
  match string_array with
  |s1::s2::s3::s4::[]  -> 
    let x = s1 |> parse |> eval_expr (VarLog.empty ()) |> fst |> pull_num in
    let y = s2 |> parse |> eval_expr (VarLog.empty ()) |> fst |> pull_num in
    let z = s3 |> parse |> eval_expr (VarLog.empty ()) |> fst |> pull_num in
    let answer = s4 |> parse |> eval_expr (VarLog.empty ()) |> fst |> pull_num in
    ([|x;y;z|], answer);
  |_-> failwith "Wrong number of values. Redo ALL of your functions"

(**[parse_lin_equation3 str] converts a string representation of a system of
   linear equations with 3 unknowns, [str], into the required arrays for
   solving*)
let parse_lin_equation2 str =  
  let string_array = String.split_on_char (',') (str) in
  match string_array with 
  |s1::s2::s3::[] -> 
    let x = s1 |> parse |> eval_expr (VarLog.empty ()) |> fst |> pull_num in
    let y = s2 |> parse |> eval_expr (VarLog.empty ()) |> fst |> pull_num in
    let answer = s3 |> parse |> eval_expr (VarLog.empty ()) |> fst |> pull_num in
    ([|x;y|], answer);
  |_-> failwith "Wrong number of values. Redo ALL of your functions"


(**[parse_lin_equation3 str] converts a string representation of a system of
   linear equations with 2 unknowns, [str], into the required arrays for
   solving*)
let print_linear_equation_answer arr num =
  if(num = 3) then
    (
      print_string ("x = " ^ string_of_float arr.(0) ^ ", ");
      print_string ("y = " ^ string_of_float arr.(1) ^ ", ");
      print_string ("z = " ^ string_of_float arr.(2)); 
      ()
    )
  else
    (
      print_string ("x = " ^ string_of_float arr.(0) ^ ", ");
      print_string ("y = " ^ string_of_float arr.(1));
      ()
    )

(**[linear_solver_helper s] solves the system of equations with [s] unknowns
   which is input by the user*)
let linear_solver_helper s =
  match s with
  |"three" ->  begin
      print_string linear_prompt;
      let eq1 = read_line() |> parse_lin_equation3 in
      print_string 
        "Please input equation 2 in the same format:\n\nequation 2> ";
      let eq2 = read_line() |> parse_lin_equation3 in
      print_string 
        "Please input equation 3 in the same format:\n\nequation 3> ";
      let eq3 = read_line() |> parse_lin_equation3 in
      let matrix = [|(fst eq1); (fst eq2); (fst eq3)|] in
      let target_vector = [|(snd eq1);(snd eq2);(snd eq3)|] in
      let answer = solve_linear_equation matrix target_vector 3 in
      print_linear_equation_answer answer 3;
      "\nSolved!" 
    end
  | "two" -> begin
      print_string linear_prompt;
      let eq1 = read_line() |> parse_lin_equation2 in
      print_string 
        "Please input equation 2 in the same format:\n\nequation 2> ";
      let eq2 = read_line() |> parse_lin_equation2 in
      let matrix = [|(fst eq1); (fst eq2);|] in
      let target_vector = [|(snd eq1);(snd eq2)|] in
      let answer = solve_linear_equation matrix target_vector 2 in
      print_linear_equation_answer answer 2;
      "\nSolved!" 
    end
  | _-> failwith "Invalid input. Only 3x3 or 2x2 systems are supported"

(**module [Coords] represents the maximum and minimum x and y coordinates
   of a graphing window*)
module Coords = struct
  type t = 
    { mutable x_min : float; 
      mutable x_max : float; 
      mutable y_min : float; 
      mutable y_max : float } 

  (** [empty] represents the coordinates of a newly opened graphing window*)
  let empty : t = 
    { x_min = (~-.10.); x_max = 10.; y_min = (~-.10.); y_max = 10. }

  (**[update_coords old_c xi xa yi ya] updates the coordinates of [old_c] to
     the new x min and max, xi and xa, and the new y min and max, yi and ya.*)
  let update_coords old_c xi xa yi ya = 
    old_c.x_min <- xi; old_c.x_max <- xa; 
    old_c.y_min <- yi; old_c.y_max <- ya; ()

end

(** [State] is the module that holds type [t], which is the representation
    of the current environment that holds variable values. In it are also 
    functions that help update this environment*)
module State = struct 
  type t = (VarLog.var list) ref
  let empty : t = ref []
  let update_state st_old inp = 
    let st = !st_old in 
    let rec insert st tup = 
      match st with 
      | [] -> [tup]
      | h :: t -> begin 
          if (fst h) = (fst tup) then tup :: t 
          else h :: insert t tup
        end in 
    let rec merge st inp = 
      match inp with 
      | [] -> st 
      | h :: t -> merge (insert st h) t
    in 
    st_old := merge st inp
end

(**[set_scale coords] sets the coordinates of a graphing window to [coords]*)
let set_scale coords = 
  let xmin = print_string "SET MIN X> "; read_line () |> float_of_string in
  let xmax = print_string "SET MAX X> "; read_line () |> float_of_string in
  let ymin = print_string "SET MIN Y> "; read_line () |> float_of_string in
  let ymax = print_string "SET MAX Y> "; read_line () |> float_of_string in
  Coords.update_coords coords xmin xmax ymin ymax; ()

(*BISECT-IGNORE-END*)

let interp (s : string) : string =
  match s |> parse_phrase with 
  | Expr e -> e |> eval_expr (ref (!State.empty, [])) |> fst |> string_of_val
  | Defn d -> begin 
      let vl' = ref (!State.empty, []) in
      let res = s |> Main_lang.parse |> eval_init vl' in 
      State.update_state State.empty (VarLog.expose (snd res)); (fst res) |> string_of_val
    end
  | Solver s -> linear_solver_helper s
  | SetScale -> set_scale Coords.empty; ""
  | Graph e ->
    let em = Coords.empty in 
    Graphing.graph em.x_min em.x_max em.y_min em.y_max (e |> eval_graph); 
    "Graphed"
  | Newton e -> newton_helper e
  | Exec e -> exec_helper (e |> eval_expr (VarLog.empty ()) |> fst)
  | _ -> raise No_keyword