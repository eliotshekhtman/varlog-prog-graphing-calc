open Ast 
open Array
open VarLog

(** [find_lbls vl d] is the VarLog [vl] with all the labels
    in [d] bound to it *)
let rec find_lbls vl = function 
  | DEnd -> vl 
  | DReturn (e,d) -> find_lbls vl d
  | DDisp (_, d) -> find_lbls vl d 
  | DGraph (_, d) -> find_lbls vl d
  | DAssign (_, _, d) -> find_lbls vl d 
  | DPrompt (_, d) -> find_lbls vl d
  | DIf (_, _, _, d) -> find_lbls vl d
  | DGoto (_, d) -> find_lbls vl d 
  | DGotoSub (_, d) -> find_lbls vl d
  | DLabel (s, d) -> VarLog.bind_lbl s d vl; find_lbls vl d
  | DMatrixSet (_, _, _, _, d) -> find_lbls vl d
  | DOutput (_, _, _, _, d) -> find_lbls vl d
  | DLine (_, _, _, _, d) -> find_lbls vl d
  | DFunction (_, _, _, d) -> find_lbls vl d
  | DDefStruct (_, _, _, d) -> find_lbls vl d
  | DStructSet (_, _, _, d) -> find_lbls vl d
  | DWhile (_, _, d) -> find_lbls vl d
  | DDefClass (_, _, _, d) -> find_lbls vl d
  | DObjSet (_, _, _, d) -> find_lbls vl d
  | _ -> failwith "Unimplemented: find_lbls"

(*BISECT-IGNORE-BEGIN*)


(** [has_return d] is if there is a [RETURN] 
    in [d] *)
let rec has_return = function 
  | DEnd -> false
  | DReturn (e,d) -> true
  | DGraph (_, d) -> has_return d
  | DDisp (_, d) -> has_return d
  | DAssign (_, _, d) -> has_return d
  | DPrompt (_, d) -> has_return d
  | DIf (_, d1, d2, d3) -> 
    has_return d1 || has_return d2 || has_return d3
  | DGoto (_, d) -> has_return d 
  | DGotoSub (_, d) -> has_return d
  | DLabel (_, d) -> has_return d
  | DMatrixSet (_, _, _, _, d) -> has_return d
  | DOutput (_, _, _, _, d) -> has_return d
  | DLine (_, _, _, _, d) -> has_return d
  | DFunction (_, _, _, d) -> has_return d
  | DDefStruct (_, _, _, d) -> has_return d
  | DStructSet (_, _, _, d) -> has_return d
  | DObjSet (_, _, _, d) -> has_return d
  | DDefClass (_, _, _, d) -> has_return d
  | DWhile (_, _, d) -> has_return d
  | _ -> failwith "Unimplemented: has_return"

(** [atoc d] is if there is an abrupt transfer of control
    in [d], such as a [GOTO] or a [RETURN] which indicates that
    there has been an Abrupt Transfer of Control (atoc). *)
let rec atoc = function 
  | DEnd -> false
  | DReturn (e,d) -> true
  | DGraph (_, d) -> atoc d
  | DDisp (_, d) -> atoc d
  | DAssign (_, _, d) -> atoc d
  | DPrompt (_, d) -> atoc d
  | DIf (_, d1, d2, d3) ->
    has_return d1 || has_return d2 || atoc d3
  | DGoto (_, d) -> true 
  | DGotoSub (_, d) -> atoc d
  | DLabel (_, d) -> atoc d
  | DMatrixSet (_, _, _, _, d) -> atoc d
  | DOutput (_, _, _, _, d) -> atoc d
  | DLine (_, _, _, _, d) -> atoc d
  | DFunction (_, _, _, d) -> atoc d
  | DDefStruct (_, _, _, d) -> atoc d
  | DStructSet (_, _, _, d) -> atoc d
  | DObjSet (_, _, _, d) -> atoc d
  | DDefClass (_, _, _, d) -> atoc d
  | DWhile (_, _, d) -> atoc d
  | _ -> failwith "Unimplemented: atoc"

(*BISECT-IGNORE-END*)

(** [fact n] is the factorial of float [n] *)
let rec fact n = 
  if n < 0. then failwith "not integer"
  else if n = 0. then 1. else n *. fact (n -. 1.)

(** [is_value e] is whether expr [e] is a valid value *)
let is_value : expr -> bool = function
  | Val _ -> true
  | Var _ -> failwith "precondition violated: variable"
  | _ -> false

(** [replace lst k v] is [lst] with [(k, v)] inserted, and if 
    [lst] already had a [(k, _)] tuple, it is replaced with 
    [(k, v)] *)
let rec replace lst k v = 
  match lst with 
  | [] -> [(k, v)]
  | h :: t -> if (fst h) = k then (k, v) :: t else h :: replace t k v

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.parse_expr Lexer.read lexbuf in
  ast

(** [is_int f] is whether a float [f] is a float representation of an integer *)
let is_int f = 
  let f' = f |> int_of_float |> float_of_int in 
  if f = f' then true 
  else false


let print_matrix arr = 
  let printed = ref "" in
  let row_length = Array.length arr in
  let col_length = Array.length arr.(0) in
  let print_matrix_helper arr str = 
    for i = 0 to row_length - 1 do
      for j = 0 to col_length - 1 do
        str := !str ^ " " ^ (arr.(i).(j) |> string_of_float) ;
      done;
      str := !str ^ "\n";
    done;
    str
  in 
  let stringified = printed |> print_matrix_helper arr in
  !stringified

(** [scalar_mult f m] is the result of scalar multiplication of float [f] with 
    matrix [m] *)
let scalar_mult a (arr: float array array) : float array array = 
  let arr' = Array.make_matrix (Array.length arr) (Array.length arr.(0)) 0. in
  for i = 0 to (arr|>length) - 1 do
    for j = 0 to (arr.(0)|>length) - 1 do
      arr'.(i).(j) <- a *. arr.(i).(j);
    done;
  done;
  arr'

(** [add_mat a1 a2] returns the 2d float array which is the result of  of adding
    multiplication of float [f] with matrix [m] *)
let add_mat a1 a2  = 
  if (Array.length a1 <> Array.length a2 || 
      Array.length a1.(0) <> Array.length a1.(0)) then
    failwith "matrices must have the same dimensions to be added" 
  else
    let arr' = Array.make_matrix (Array.length a1) (Array.length a1.(0)) 0. in
    for i = 0 to (a1|>length) - 1 do
      for j = 0 to (a1.(0)|>length) - 1 do
        arr'.(i).(j) <-  a1.(i).(j) +. a2.(i).(j);
      done;
    done;
    arr'

(** [matrix_mult m1 m2] is the product of matrix [m1] and matrix [m2] *)
let matrix_mult arr1 arr2 : float array array = 
  let r1 = Array.length arr1 in
  let c1 = Array.length arr1.(0) in
  let r2 = Array.length arr2 in
  let c2 = Array.length arr2.(0) in


  (*columns in first == rows in second*)
  if(c1 <> r2) then 
    failwith "Error: columns of first matrix not equal to rows of second " 
  else 
    let new_matrix = Array.make_matrix r1 c2 0. in
    for i = 0 to r1-1 do
      for j = 0 to c2-1 do
        let sum = ref 0. in
        for k = 0 to c1-1 do
          sum := !sum +. arr1.(i).(k) *. arr2.(k).(j);
          new_matrix.(i).(j) <- !sum;
        done ;
      done ;
    done;
    new_matrix

let string_of_val (v : value) : string =
  match v with
  | Num i -> 
    if is_int i then i |> int_of_float |> string_of_int
    else string_of_float i
  | Bool b -> string_of_bool b
  | Str s -> s
  | Closure (_, _,_) -> "<closure>"
  | Matrix arr -> print_matrix arr
  | Null -> ""
  | Struct _ -> "<struct>"
  | Built _ -> "<built>"
  | VarMat _ -> "<varmat>"
  | Color _ -> "<color>"
  | Class _ -> "<class>"
  | Object _ -> "<object>"

(*BISECT-IGNORE-BEGIN*)

(** [is_value e] is whether [e] is a value. *)
let is_value_graph : expr -> bool = function
  | Val _ -> true
  | Var "x" -> true
  | _ -> false

(** [get_val v] is the number stored in value [v] *)
let get_val v = function
  | Val (Num n) -> n
  | Var "x" -> v 
  | _ -> failwith "precondition violated"

(** [step e] takes a single step in the graphing of [e]*)
let rec step_graph v = function
  | Val _ -> failwith "Does not step"
  | Var _ -> failwith "precondition violated: variable"
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

let rec eval_graph (e : expr) (v : float) : float = 
  match e with
  | Val (Num n) -> n
  | Var "x" -> v
  | _ -> eval_graph (e |> step_graph v) v

(*BISECT-IGNORE-END*)

(** [integrate b1 b2 acc f] is the definite integral of function [f] between
    bounds [b1] and [b2] *)
let rec integrate a b acc fn = 
  if(a >= b) then acc 
  else
    let area = acc +. 0.000001 *. fn (a+.0.000001) in
    integrate (a+.0.000001) b area fn 

let derive a fn = 
  (fn (a+.0.000000001) -. fn (a-.0.000000001)) /. 0.000000002

(** [add_helper v1 v2] is the sum of [v1] and [v2].
    Requires: [v1] and [v2] are both values. *)
let add_helper v1 v2 = 
  match v1, v2 with 
  | Num a, Num b -> Num (a +. b)
  | Str a, Str b -> Str (a ^ b)
  | Num a, Str b -> Str ((v1 |> string_of_val) ^ b)
  | Str a, Num b -> Str (a ^ (v2 |> string_of_val))
  | Matrix a, Matrix b -> failwith "somebody pls implement matrix addition?"
  | _ -> failwith "precondition violated: add types"

(** [eq_helper v1 v2] is whether [v1] and [v2] are equal.
    Requires: [v1] and [v2] are both values. *)
let eq_helper v1 v2 = 
  match v1, v2 with 
  | Num a, Num b -> Bool (a = b)
  | Str a, Str b -> Bool (a = b)
  | Bool a, Bool b -> Bool (a = b) 
  | _ -> Bool false

(** [lt_helper v1 v2] is whether [v1] is less than [v2]. *)
let lt_helper v1 v2 = 
  match v1, v2 with 
  | Num a, Num b -> Bool (a < b)
  | Str a, Str b -> Bool (a < b)
  | Bool a, Bool b -> Bool (a < b) 
  | _ -> Bool false

(** [gt_helper v1 v2] is whether [v1] is greater than [v2]. *)
let gt_helper v1 v2 = 
  match v1, v2 with 
  | Num a, Num b -> Bool (a > b)
  | Str a, Str b -> Bool (a > b)
  | Bool a, Bool b -> Bool (a > b) 
  | _ -> Bool false

(** [not_val v] is the "not" operator acted on [v]. *)
let not_val v = 
  match v with 
  | Bool b -> Bool (not b)
  | _ -> failwith "precondition violated: not bool"

(**[substitute vl e] is the value that [e] represents in the
   association list [vl]. *)
let substitute vl s = 
  try List.assoc s vl with _ -> 
    failwith ("precondition violated: unbound var " ^ s)

let pull_num = function 
  | Num n -> n 
  | _ -> failwith "precondition violated: not a number"  

let rec eval_expr vl e = 
  match e with
  | Val v -> (v, vl) 
  | PreString s -> 
    let len = String.length s - 2 in 
    ((Str (String.sub s 1 len)), vl)
  | Var v -> ((substitute vl v), vl) 
  | Uniop (uop, e) -> eval_uop vl uop e
  | Binop (bop, e1, e2) -> eval_bop vl bop e1 e2 
  | Ternop (top, (e1, e2), e3) -> eval_top vl top e1 e2 e3
  | Derivative (der, e1, e2) -> eval_deriv vl der e1 e2
  | Boolop (boop, e1, e2) -> eval_boop vl boop e1 e2
  | GetKey -> 
    (* learned how to do this here: 
       https://caml.inria.fr/pub/docs/oreilly-book/html/book-ora050.html *)
    let c = Graphics.wait_next_event [Graphics.Key_pressed] in 
    (Str (Char.escaped c.Graphics.key), vl)
  | Prompt -> begin
      print_string ">> ";
      let s = try (read_line () |> parse) with _ -> Val (Str "") in
      s |> eval_expr vl
    end
  | MakeMatrix (a,b) -> eval_matrix a b vl 
  | MakeVarMat (a, b) -> eval_varmat a b vl
  | MatrixGet (m, a, b) -> eval_matrixget vl m a b
  | RandInt (lb, ub) -> eval_randint vl lb ub
  | StructGet (n, s) -> eval_structget n s vl
  | ObjectGet (n, s) -> eval_objectget n s vl
  | Ternary (guard, e1, e2) -> eval_ternary vl guard e1 e2 
  | Application(n, es) -> eval_app n es vl
  | ComplApp(e, es) -> eval_complapp e es vl
  | _ -> failwith "lol right"

(** [eval_add_helper args es clos vl_ext] *)
and eval_app_helper args es clos vl_ext = 
  if List.length args != List.length es 
  then failwith "precondition violated: wrong # of args in function"
  else
    let rec bind_args args ex vl vl_ext = 
      match args, ex with 
      | [], [] -> vl 
      | h1 :: t1, h2 :: t2 ->
        let val_arg = fst (eval_expr vl_ext h2) in 
        bind_args t1 t2 (replace vl h1 val_arg) vl_ext
      | _ -> failwith "improper # args checking"
    in 
    let new_vl = bind_args args es clos vl_ext in 
    new_vl  

(** [eval_app n es vl] is the evaluation of the function bound to [n] in 
    VarLog [vl] with arguments [es] *)
and eval_app n es vl =
  let value = substitute vl n in
  match value with 
  | Class (cargs, body) -> 
    let new_vl = eval_app_helper cargs es [] vl in 
    let vl' = ref (new_vl, []) in 
    let res = eval body (find_lbls vl' body) in 
    (Object (res |> snd), vl)
  | Closure (args, d, vl_closure) ->  
    let new_vl = eval_app_helper args es vl_closure vl in
    let x = ref (new_vl,[]) in
    eval d (find_lbls x d)
  | Struct (cargs, body) -> begin 
      let new_vl = eval_app_helper cargs es vl [] in
      let res = eval body (ref (new_vl, [])) |> snd in
      (Built res, vl)
    end
  | _ -> failwith "Can't do function application on non-function" 

(** [eval_complapp e es vl] is the result of evaluating a closure given 
    in [e] with arguments [es] in VarLog [vl] *)
and eval_complapp e es vl = 
  let clsr = eval_expr vl e |> fst in 
  match clsr with 
  | Closure (args, d, vl_closure) -> 
    let new_vl = eval_app_helper args es vl_closure vl in
    let x = ref (new_vl,[]) in
    eval d (find_lbls x d) 
  | _ -> failwith "Can't do function application on non-function"

(** [eval_varmat a b vl] is the result of evaluating a [VarMat] type. It creates
    an [a] by [b] matrix that can have any value in its cells *)
and eval_varmat a b vl = 
  let r1 = eval_expr vl a in
  let r2 = eval_expr (snd r1) b in
  match fst r1, fst r2 with
  | Num a, Num b  -> 
    if (is_int a = false || is_int b = false) then 
      failwith "cannot have float values"
    else 
      ((VarMat (Array.make_matrix (a|>int_of_float) 
                  (b|>int_of_float) Null)), vl)
  |_-> failwith "precondition violated: make varmat"

(** [eval_ternary vl g a b] is the result of evaluating either 
    [a] or [b], depending on the truth value returned by the
    evaluation of [g] *)
and eval_ternary vl g a b = 
  match eval_expr vl g with 
  | Bool true, vl' -> eval_expr vl' a 
  | Bool false, vl' -> eval_expr vl' b 
  | _ -> failwith "precondition violated: ternary operator guard"

(** [eval_objectget n s vl] is field [s] of object [n] *)
and eval_objectget n s vl = 
  match substitute vl n with 
  | Object vl' -> (substitute vl' s, vl)
  | _ -> failwith "precondition violated: not an object"

(** [eval_structget n s vl] is field [s] of built [n] *)
and eval_structget n s vl = 
  match substitute vl n with 
  | Built vl' -> (substitute vl' s, vl)
  | _ -> failwith "precondition violated: not a built"

(** [eval_randint vl a b] is a random integer between
    the results of evaluating [a] and [b], not including
    the upper bound *)
and eval_randint vl a b = 
  let r1 = eval_expr vl a in 
  let (r2, vl') = eval_expr (snd r1) b in 
  match fst r1, r2 with 
  | Num a, Num b -> begin 
      let lb = a |> int_of_float in 
      let ub = b |> int_of_float in 
      let v = lb + Random.int (ub-lb) in
      (Num (v |> float_of_int), vl)
    end
  | _ -> failwith "precondition violated: not numerical inputs"

(** [eval_matrix a b vl] is a matrix (2D Int Array) with length
    given by the evaluating of [a] and height given by the 
    evaluation of [b] with VarLog [vl] *)
and eval_matrix a b vl = 
  let r1 = eval_expr vl a in
  let r2 = eval_expr (snd r1) b in
  match fst r1, fst r2 with
  | Num a, Num b  -> 
    if (is_int a = false || is_int b = false) then 
      failwith "cannot have float values"
    else 
      ((Matrix (Array.make_matrix (a|>int_of_float) (b|>int_of_float) 0.)), vl)
  |_-> failwith "precondition violated: make matrix"

(** [eval_matrixget vl m a b] is the value in [m] at x location [a]
    and y location [b] *)
and eval_matrixget vl m a b = 
  let r1 = eval_expr vl m in 
  let r2 = eval_expr (snd r1) a in 
  let (r3, vl') = eval_expr (snd r2) b in
  match (fst r1), (fst r2), r3 with 
  | Matrix m, Num a, Num b -> 
    if is_int a && is_int b then 
      (Num m.(a |> int_of_float).(b |> int_of_float), vl')
    else failwith "precondition violated: float indeces"
  | VarMat m, Num a, Num b ->
    if is_int a && is_int b then 
      (m.(a |> int_of_float).(b |> int_of_float), vl')
    else failwith "precondition violated: float indeces"
  | _ -> failwith "precondition violated: matrix get"
(**[eval_boop vl boop e1 e2] evaluates the primitive expression [e1] [boop]
   [e2]. *) 
and eval_boop vl b e1 e2 = 
  let r1 = eval_expr vl e1 in 
  match b, (fst r1) with 
  | And, Bool false -> (Bool false, snd r1)
  | Or, Bool true -> (Bool true, snd r1) 
  | _ -> 
    let (r2, vl2) = eval_expr (snd r1) e2 in 
    begin 
      match b, (fst r1), r2 with 
      | And, Bool a, Bool b -> (Bool (a && b), vl2)
      | Or, Bool a, Bool b -> (Bool (a || b), vl2) 
      | Xor, Bool a, Bool b -> (Bool (a <> b), vl2)
      | _ -> failwith "Unimplemented: boop"
    end
(**[eval_expr vl bop e1 e2] evaluates the primitive expression [e1] [bop] [e2].
*)  
and eval_bop vl b e1 e2 = 
  let r1 = eval_expr vl e1 in 
  let (r2, vl2) = eval_expr (snd r1) e2 in 
  match b, (fst r1), r2 with 
  | Add, Matrix a, Matrix b -> (Matrix (add_mat a b), vl2)
  | Add, v1, v2 -> (add_helper v1 v2, vl2)
  | Subt, Num a, Num b -> (Num (a -. b), vl2)
  | Mult, Matrix a, Num b -> (Matrix (scalar_mult b a), vl2)
  | Mult, Num a, Matrix b -> (Matrix (scalar_mult a b), vl2)
  | Mult, Matrix a, Matrix b -> (Matrix (matrix_mult a b), vl2)
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
(**[eval_expr vl uop e] evaluates the primitive expression [uop] [e]. *)
and eval_uop vl u e = 
  let (r, vl') = eval_expr vl e in 
  match u, r with 
  | Not, Bool b -> (Bool (not b), vl')
  | Subt, Num n -> (Num (~-.n), vl')
  | Fact, Num n -> (Num (fact n), vl')
  | Sin, Num n -> (Num (sin n), vl')
  | Cos, Num n -> (Num (cos n), vl')
  | Tan, Num n -> (Num (tan n), vl')
  | ArcSin, Num n -> (Num (asin n), vl')
  | ArcCos, Num n -> (Num (acos n), vl')
  | ArcTan, Num n -> (Num (atan n), vl')
  | _ -> failwith "precondition violated: uop"
(**[eval_expr vl top e1 e2 e3] evaluates the primitive expression [top] [e1] 
   [e2] [e3]. *)
and eval_top vl top e1 e2 e3 = 
  let r1 = eval_expr vl e1 in 
  let r2 = eval_expr (snd r1) e2 in 
  match top, (fst r1), (fst r2) with 
  | Integral, Num a, Num b -> 
    (Num (integrate a b 0. (e3 |> eval_graph)), (snd r2))
  | _ -> failwith "precondition violated: top"
(**[eval_deriv vl der e1 e2] evaluates the derivative [der] on [e2], 
   at [e1]*)
and eval_deriv vl der e1 e2 = 
  let r1 = eval_expr vl e1 in 
  match der, (fst r1) with 
  | Der, Num a -> (Num (derive a (e2 |> eval_graph)), (snd r1))
  | _ -> failwith "precondition violated: derivative"

(**[eval d vl] evaluates definition [d] in VarLog [vl]*)
and eval d vl = 
  match d with 
  | DEnd -> (Null, VarLog.expose vl)
  | DReturn (e, d) ->  
    (e |> eval_expr (VarLog.expose vl) |> fst, VarLog.expose vl)
  | DGraph (e, d) -> eval_dgraph e d vl
  | DDisp (e, d) -> eval_disp e d vl 
  | DAssign (s, e, d) -> eval_assign s e d vl
  | DPrompt (s, d) -> eval_prompt s d vl
  | DIf (e, d1, d2, d3) -> eval_if e d1 d2 d3 vl
  | DLabel (s, d) -> eval d vl 
  | DGoto (s, d) -> eval (VarLog.find_lbl s vl) vl
  | DGotoSub (s, d) -> ignore (eval (VarLog.find_lbl s vl) vl); eval d vl 
  | DMatrixSet (m, e1, e2, e3, d) -> eval_matrixset m e1 e2 e3 d vl
  | DOutput (x, y, v, c, d) -> eval_output x y v c d vl
  | DLine (x1, y1, x2, y2, d) -> eval_line x1 y1 x2 y2 d vl
  | DFunction (name, args, body, d) -> 
    VarLog.bind name (eval_func args body vl) vl; eval d vl
  | DDefStruct (name, cargs, body, d) -> 
    eval_structdef name cargs body d vl
  | DDefClass (name, cargs, body, d) -> 
    eval_classdef name cargs body d vl
  | DStructSet (n, s, e, d) -> 
    eval_set n s e d vl
  | DObjSet (n, s, e, d) -> 
    eval_set n s e d vl
  | DWhile (e, d1, d2) -> eval_while false e d1 d2 vl
  | _ -> failwith "Unimplemented: eval"
(**[eval_while evaluated_once e d1 d2 vl] evaluates a while loop*)
and eval_while evaluated_once e d1 d2 vl = 
  match e |> eval_expr (VarLog.expose vl) |> fst with 
  | Bool false -> 
    if (not evaluated_once || d1 |> atoc |> not) then eval d2 vl
    else (Null, VarLog.expose vl)
  | Bool true -> 
    let r = eval d1 vl in 
    if (not evaluated_once || d1 |> atoc |> not) 
    then eval_while true e d1 d2 vl
    else r
  | _ -> failwith "precondition violated: while guard"

(** [eval_classdef name cargs body d vl] evaluates a class and places the 
    class into the [VarLog] for future reference *)
and eval_classdef name cargs body d vl = 
  let c = Class (cargs, body) in 
  VarLog.bind name c vl;
  eval d vl

(**[eval_set n s e d vl] evaluates a dset*)
and eval_set n s e d vl = 
  match VarLog.find n vl with 
  | Some (Built vl') -> begin 
      let v = e |> eval_expr (VarLog.expose vl) |> fst in 
      VarLog.bind n (Built (replace vl' s v)) vl; eval d vl
    end
  | Some (Object vl') -> begin 
      let v = e |> eval_expr (VarLog.expose vl) |> fst in 
      VarLog.bind n (Object (replace vl' s v)) vl; eval d vl
    end
  | _ -> failwith "precondition violated: not a built"

(**[eval_structdef name cargs body d vl] creates a struct with arguments [cargs]
   and body [body], and stores it in VarLog [vl] bound to identifier [name]*)
and eval_structdef name cargs body d vl = 
  let s = Struct (cargs, body) in 
  VarLog.bind name s vl;
  eval d vl

(**[eval_func args body vl] evaluates function [body] with arguments [args] in
   VarLog [vl]*)
and eval_func args body vl = 
  Closure (args, body, (VarLog.expose vl))

(**[eval_line e1 e2 e3 e4 d vl] draws lines in the graphing space. Returns the 
   the evaluation of the [d] and [vl] that was put in by putting 
   those two through [eval d vl] again*)
and eval_line e1 e2 e3 e4 d vl = 
  let x1 = e1 |> eval_expr (VarLog.expose vl) |> fst in 
  let y1 = e2 |> eval_expr (VarLog.expose vl) |> fst in 
  let x2 = e3 |> eval_expr (VarLog.expose vl) |> fst in 
  let y2 = e4 |> eval_expr (VarLog.expose vl) |> fst in 
  match x1, y1, x2, y2 with 
  | Num f1, Num f2, Num f3, Num f4 -> begin 
      Graphing.draw_line f1 f2 f3 f4; eval d vl
    end
  | _ -> failwith "precondition violated: not numerical coordinates"

(**[eval_dgraph e d vl] sets a scale and graphs a user-defined function
   Returns the 
   the evaluation of the [d] and vl that was put in by putting those 
   two through [eval d vl] again*) 
and eval_dgraph e d vl = 
  let f = e |> eval_graph in 
  Graphing.graph_func (-10.) (10.) (-10.) (10.) 0 Graphics.black f;
  eval d vl

(** [eval_output x y v c d vl] is the evaluation of [d], but it
    displays the evaluation of [v] at coordinates [x] [y] with
    color [c] *)
and eval_output x y v c d vl = 
  let x' = x |> eval_expr (VarLog.expose vl) |> fst in 
  let y' = y |> eval_expr (VarLog.expose vl) |> fst in 
  let v' = v |> eval_expr (VarLog.expose vl) |> fst in 
  let c' = c |> eval_expr (VarLog.expose vl) |> fst in 
  match x', y' with 
  | Num a, Num b -> begin 
      Graphing.output a b (v' |> string_of_val) c'; eval d vl
    end
  | _ -> failwith "precondition violated: not numerical coordinates"

(** [eval_matrixset m a b v d vl] is the evaluation of [d], but it
    sets the value of [m] at coordinates [a] [b] to the evaluation 
    of [v] *)
and eval_matrixset m a b v d vl = 
  let m' = m |> eval_expr (VarLog.expose vl) |> fst in 
  let a' = a |> eval_expr (VarLog.expose vl) |> fst in
  let b' = b |> eval_expr (VarLog.expose vl) |> fst in
  let v' = v |> eval_expr (VarLog.expose vl) |> fst in
  match m', a', b', v' with 
  | Matrix m, Num a, Num b, Num v -> begin 
      if (is_int a && is_int b) then 
        (m.(a |> int_of_float).(b |> int_of_float) <- v; eval d vl)
      else failwith "precondition violated: float indices"
    end
  | VarMat m, Num a, Num b, v -> begin 
      if (is_int a && is_int b) then 
        (m.(a |> int_of_float).(b |> int_of_float) <- v; eval d vl)
      else failwith "precondition violated: float indices"
    end
  | _ -> failwith "precondition violated: matrix set"

(**[eval_disp e d vl] prints out whatever string/integer/boolean/valid input
   that you pass in. 
   Returns the evaluation of the same definition and [vl] that was put in by 
   putting those two through [eval d vl] again*) 
and eval_disp e d vl = 
  let v = e |> eval_expr (VarLog.expose vl) |> fst |> string_of_val in 
  print_endline v;
  eval d vl

(**[eval_prompt s d vl] prompts the user to input some value through the REPL
    It takes their input so it can be used later on by calling 
    [eval_assign s e d vl]*)
and eval_prompt s d vl = 
  print_string (s ^ " >> ");
  let e = try (read_line () |> parse) with _ -> Val (Str "") in
  eval_assign s e d vl

(**[eval_assign s e d vl] takes in a variable and an expression and sets
   the variable equal to the value of the expression. Returns [eval d vl]
   so it calls the big [eval] function again*)
and eval_assign s e d vl =
  let v = e |> eval_expr (VarLog.expose vl) |> fst in 
  match v with 
  | Matrix m -> 
    VarLog.bind s (Matrix (Array.map Array.copy m)) vl; eval d vl
  | _ -> VarLog.bind s v vl; eval d vl

(**[eval_if e d1 d2 d3 vl] evaluates if statements. [d1] corresponds
   to the first if condition. [d2] corresponds to the "then" sections of an
   if-else block. And [d3] corresponds to the body contained in the else block*)
and eval_if e d1 d2 d3 vl =
  match e |> eval_expr (VarLog.expose vl) |> fst with 
  | Bool true -> 
    let res = eval d1 vl in
    if(d1 |> atoc |> not) then eval d3 vl else res
  | Bool false -> 
    let res = eval d2 vl in
    if(d2 |> atoc |> not) then eval d3 vl else res
  | _ -> failwith "precondition violated: if guard"

let eval_init vl d = eval d (find_lbls vl d)