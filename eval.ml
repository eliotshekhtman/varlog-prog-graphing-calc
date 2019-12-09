open Ast 
open Array
open VarLog

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
  | DInstantiateStruct (_, _, _, d) -> find_lbls vl d
  | DStructSet (_, _, _, d) -> find_lbls vl d
  | DWhile (_, _, d) -> find_lbls vl d
  | _ -> failwith "Unimplemented: find_lbls"

let rec has_goto = function 
  | DEnd -> false
  | DReturn (e,d) -> true
  | DGraph (_, d) -> has_goto d
  | DDisp (_, d) -> has_goto d
  | DAssign (_, _, d) -> has_goto d
  | DPrompt (_, d) -> has_goto d
  | DIf (_, _, _, d) -> has_goto d
  | DGoto (_, d) -> true 
  | DGotoSub (_, d) -> has_goto d
  | DLabel (_, d) -> has_goto d
  | DMatrixSet (_, _, _, _, d) -> has_goto d
  | DOutput (_, _, _, _, d) -> has_goto d
  | DLine (_, _, _, _, d) -> has_goto d
  | DFunction (_, _, _, d) -> has_goto d
  | DDefStruct (_, _, _, d) -> has_goto d
  | DInstantiateStruct (_, _, _, d) -> has_goto d
  | DStructSet (_, _, _, d) -> has_goto d
  | _ -> failwith "Unimplemented: has_goto"

let rec fact n = 
  if n < 0. then failwith "not integer"
  else if n = 0. then 1. else n *. fact (n -. 1.)

let is_value : expr -> bool = function
  | Val _ -> true
  | Var _ -> failwith "precondition violated: variable"
  | _ -> false

let rec replace lst k v = 
  match lst with 
  | [] -> [(k, v)]
  | h :: t -> if (fst h) = k then (k, v) :: t else h :: replace t k v

(** [parse s] parses [s] into an AST. *)
let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.parse_expr Lexer.read lexbuf in
  ast

let is_int f = 
  let f' = f |> int_of_float |> float_of_int in 
  if f = f' then true 
  else false


let matrix = [| [|1.;2.;3.|];[|0.;1.;5.|];[|5.;6.;0.|] |]
let identity = [| [|1.;0.;0.|];[|0.;1.;0.|];[|0.;0.;1.|] |]


let firstMatrix = [| [|3.;-2.;5.|]; [|3.;0.;-4.|] |]
let secondMatrix = [| [|2.;3.|]; [|-9.;0.|];[|0.;4.|] |]

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

let scalar_mult a (arr: float array array) : float array array = 
  let arr' = Array.make_matrix (Array.length arr) (Array.length arr.(0)) 0. in
  for i = 0 to (arr|>length) - 1 do
    for j = 0 to (arr.(0)|>length) - 1 do
      arr'.(i).(j) <- a *. arr.(i).(j);
    done;
  done;
  arr'

let matrix_mult arr1 arr2 : float array array = 
  (* let printed = ref "" in *)
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

let is_value_graph : expr -> bool = function
  | Val _ -> true
  | Var "x" -> true
  | _ -> false

let get_val v = function
  | Val (Num n) -> n
  | Var "x" -> v 
  | _ -> failwith "precondition violated"

let rec step_graph v = function
  (*| Keyword _ -> failwith "precondition violated: too many keywords" *)
  | Val _ -> failwith "Does not step"
  | Var _ -> failwith "precondition violated: variable"
  (* | Ternop (top, (e1,e2), e3) when is_value e1 && is_value e2 ->
     step_top top e1 e2 e3
     | Ternop (top, (e1,e2), e3) when is_value e1 -> 
     Ternop(top, (e1, step e2), e3)
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

and step_bop v bop e1 e2 = match bop with
  | Add -> Val (Num ((get_val v e1) +. (get_val v e2)))
  | Mult -> Val (Num ((get_val v e1) *. (get_val v e2)))
  | Subt -> Val (Num ((get_val v e1) -. (get_val v e2)))
  | Div -> Val (Num ((get_val v e1) /. (get_val v e2)))
  | Pow -> Val (Num ((get_val v e1) ** (get_val v e2)))
  | _ -> failwith "precondition violated: bop"

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
  | Matrix a, Matrix b -> failwith "somebody pls implement matrix addition?"
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
  try List.assoc s vl with _ -> 
    failwith ("precondition violated: unbound var " ^ s)

let pull_num = function 
  | Num n -> n 
  | _ -> failwith "precondition violated: not a number"


let rec find_id n' vl' =
  match vl' with
  |[]-> None
  |h::t -> if(fst h = n') then Some (snd h) else
      find_id n' t


let rec eval_expr vl e = 
  match e with
  (*| Keyword _ -> failwith "precondition violated: too many keywords" *)
  | Val v -> (v, vl) 
  | PreString s -> 
    let len = String.length s - 2 in 
    ((Str (String.sub s 1 len)), vl)
  | Var v -> ((substitute vl v), vl) 
  | Uniop (uop, e) -> eval_uop vl uop e
  | Binop (bop, e1, e2) -> eval_bop vl bop e1 e2 
  | Ternop (top, (e1, e2), e3) -> eval_top vl top e1 e2 e3
  | Derivative (der, e1, e2) -> eval_deriv vl der e1 e2
  (* | Function (xs, e1) -> eval_func xs e1 vl *)
  | Boolop (boop, e1, e2) -> eval_boop vl boop e1 e2
  | GetKey -> 
    (* learned how to do this here: 
       https://caml.inria.fr/pub/docs/oreilly-book/html/book-ora050.html *)
    let c = Graphics.wait_next_event [Graphics.Key_pressed] in 
    (Str (Char.escaped c.Graphics.key), vl)
  | Prompt -> begin
      print_string ">> ";
      let s = read_line() in
      s |> parse |> eval_expr vl
    end
  | MakeMatrix (a,b) -> eval_matrix a b vl 
  | MakeVarMat (a, b) -> eval_varmat a b vl
  | MatrixGet (m, a, b) -> eval_matrixget vl m a b
  | RandInt (lb, ub) -> eval_randint vl lb ub
  | StructGet (n, s) -> eval_structget n s vl
  | Ternary (guard, e1, e2) -> eval_ternary vl guard e1 e2 
  | Application(n, es) -> eval_app n es vl
  | _ -> failwith "lol right"

and eval_app n es vl =
  let value = substitute vl n in
  match value with 
  | Closure (args, d, vl_closure) ->  
    if(List.length args <> List.length es) then 
      failwith "Number of function arguments not consistent"
    else 
      let rec bind_arguments args' es' vl_closure' =
        match args', es' with
        |[],[] -> vl_closure'
        |h1::t1, h2::t2 ->
          let value_of_arg = 
            begin match fst (eval_expr vl h2) with
              | v -> v 
              | _ -> failwith "Argument is not an expression"
            end
          in bind_arguments t1 t2 (replace vl_closure' h1 value_of_arg)
        |_->failwith ""
      in let new_vl = bind_arguments args es vl_closure in
      let x = ref (new_vl,[]) in
      eval d (find_lbls x d)
  | Struct (cargs, body) -> begin 
      if List.length cargs != List.length es 
      then failwith "precondition violated: wrong # of args in constructor"
      else
        let rec eval_xe xe cargs acc = 
          match xe, cargs with 
          | [], [] -> acc 
          | h :: t, a :: t' -> 
            let r = eval_expr vl h |> fst in 
            eval_xe t t' ((a, r) :: acc)
          | _ -> failwith "improper # args checking"
        in 
        let res = eval body (ref (eval_xe es cargs [], [])) |> snd in
        (Built res, vl)
    end
  | _ -> failwith "Can't do function application on non-function" 



(*  match (e1,env,st) |> eval_expr with 
    | (RValue (VClosure (xs, exp, env_closure)), st) -> begin
      if List.length xs <> List.length xe then 
        ((RException (VString "Application: wrong number of arguments"), st))
      else 
        let rec bind_params xs' xe' env_closure' st' = begin
          match xs', xe' with
          | [], []-> env_closure'
          | h1::t1, h2 :: t2 -> 
            let value = 
              begin 
                match (h2, env, st') |> eval_expr with
                | (RValue v, st) -> v
                | _ -> failwith "error, not a value" 
              end in bind_params t1 t2 (Binding.bind h1 value env_closure') st'
          |_ -> failwith "error with your fn"
        end
        in let new_env = bind_params xs xe env_closure st in
        eval_expr (exp, new_env, st)
    end*)

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

and eval_ternary vl g a b = 
  match eval_expr vl g with 
  | Bool true, vl' -> eval_expr vl' a 
  | Bool false, vl' -> eval_expr vl' b 
  | _ -> failwith "precondition violated: ternary operator guard"

and eval_structget n s vl = 
  match substitute vl n with 
  | Built vl' -> (substitute vl' s, vl)
  | _ -> failwith "precondition violated: not a built"

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
      | _ -> failwith "unimplemented"
    end
and eval_bop vl b e1 e2 = 
  let r1 = eval_expr vl e1 in 
  let (r2, vl2) = eval_expr (snd r1) e2 in 
  match b, (fst r1), r2 with 
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
and eval_top vl top e1 e2 e3 = 
  let r1 = eval_expr vl e1 in 
  let r2 = eval_expr (snd r1) e2 in 
  match top, (fst r1), (fst r2) with 
  | Integral, Num a, Num b -> 
    (Num (integrate a b 0. (e3 |> eval_graph)), (snd r2))
  | _ -> failwith "precondition violated: top"
and eval_deriv vl der e1 e2 = 
  let r1 = eval_expr vl e1 in 
  match der, (fst r1) with 
  | Der, Num a -> (Num (derive a (e2 |> eval_graph)), (snd r1))
  | _ -> failwith "precondition violated: derivative"

(* Definitions! *)

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
  | DGotoSub (s, d) -> eval (VarLog.find_lbl s vl) vl; eval d vl (* NOTE: probably have to edit this, goto, and if so that return will actually return and not continue evaluating *)
  | DMatrixSet (m, e1, e2, e3, d) -> eval_matrixset m e1 e2 e3 d vl
  | DOutput (x, y, v, c, d) -> eval_output x y v c d vl
  | DLine (x1, y1, x2, y2, d) -> eval_line x1 y1 x2 y2 d vl
  | DFunction (name, args, body, d) -> 
    VarLog.bind name (eval_func args body vl) vl; eval d vl
  | DDefStruct (name, cargs, body, d) -> 
    eval_structdef name cargs body d vl
  | DInstantiateStruct (s, name, xe, d) -> 
    eval_struct s name xe vl d
  | DStructSet (n, s, e, d) -> 
    eval_structset n s e d vl
  | DWhile (e, d1, d2) -> eval_while false e d1 d2 vl
  | _ -> failwith "Unimplemented: eval"

and eval_while evaluated_once e d1 d2 vl = 
  match e |> eval_expr (VarLog.expose vl) |> fst with 
  | Bool false -> 
    if (not evaluated_once || d1 |> has_goto |> not) then eval d2 vl
    else (Null, VarLog.expose vl)
  | Bool true -> 
    let r = eval d1 vl in 
    if (not evaluated_once || d1 |> has_goto |> not) 
    then eval_while true e d1 d2 vl
    else r
  | _ -> failwith "precondition violated: while guard"


and eval_structset n s e d vl = 
  match VarLog.find n vl with 
  | Some (Built vl') -> begin 
      let v = e |> eval_expr (VarLog.expose vl) |> fst in 
      VarLog.bind n (Built (replace vl' s v)) vl; eval d vl
    end
  | _ -> failwith "precondition violated: not a built"

and eval_struct s n xe vl_full d = 
  let vl = VarLog.expose vl_full in
  match substitute vl n with 
  | Struct (cargs, body) -> begin 
      if List.length cargs != List.length xe 
      then failwith "precondition violated: wrong # of args in constructor"
      else
        let rec eval_xe xe cargs acc = 
          match xe, cargs with 
          | [], [] -> acc 
          | h :: t, a :: t' -> 
            let r = eval_expr vl h |> fst in 
            eval_xe t t' ((a, r) :: acc)
          | _ -> failwith "improper # args checking"
        in 
        let res = eval body (ref (eval_xe xe cargs [], [])) |> snd in
        VarLog.bind s (Built res) vl_full; eval d vl_full
    end
  | _ -> failwith "precondition violated: not a struct"

and eval_structdef name cargs body d vl = 
  let s = Struct (cargs, body) in 
  VarLog.bind name s vl;
  eval d vl

and eval_func args body vl = 
  Closure (args, body, (VarLog.expose vl))

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

and eval_dgraph e d vl = 
  let f = e |> eval_graph in 
  Graphing.graph_func (-10.) (10.) (-10.) (10.) 0 Graphics.black f;
  eval d vl

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

and eval_matrixset m a b v d vl = 
  let m' = m |> eval_expr (VarLog.expose vl) |> fst in 
  let a' = a |> eval_expr (VarLog.expose vl) |> fst in
  let b' = b |> eval_expr (VarLog.expose vl) |> fst in
  let v' = v |> eval_expr (VarLog.expose vl) |> fst in
  match m', a', b', v' with 
  | Matrix m, Num a, Num b, Num v -> begin 
      if (is_int a && is_int b) then 
        (m.(a |> int_of_float).(b |> int_of_float) <- v; eval d vl)
      else failwith "precondition violated: float indeces"
    end
  | VarMat m, Num a, Num b, v -> begin 
      if (is_int a && is_int b) then 
        (m.(a |> int_of_float).(b |> int_of_float) <- v; eval d vl)
      else failwith "precondition violated: float indeces"
    end
  | _ -> failwith "precondition violated: matrix set"

and eval_disp e d vl = 
  let v = e |> eval_expr (VarLog.expose vl) |> fst |> string_of_val in 
  print_endline v;
  eval d vl

and eval_prompt s d vl = 
  print_string (s ^ " >> ");
  let e = read_line () |> parse in
  eval_assign s e d vl

and eval_assign s e d vl =
  let v = e |> eval_expr (VarLog.expose vl) |> fst in 
  match v with 
  | Matrix m -> 
    VarLog.bind s (Matrix (Array.map Array.copy m)) vl; eval d vl
  | _ -> VarLog.bind s v vl; eval d vl
and eval_if e d1 d2 d3 vl =
  match e |> eval_expr (VarLog.expose vl) |> fst with 
  | Bool true -> 
    eval d1 vl; 
    if(d1 |> has_goto |> not) then eval d3 vl else (Null, VarLog.expose vl)
  | Bool false -> 
    eval d2 vl; 
    if(d2 |> has_goto |> not) then eval d3 vl else (Null, VarLog.expose vl)
  | _ -> failwith "precondition violated: if guard"

(** [string_of_expr e] converts [e] to a string.
    Requires: [e] is a value. *)
let string_of_expr (e : expr) : string =
  match e with
  | Val (Num f) -> string_of_float f
  | Val (Bool b) -> string_of_bool b
  | Val (Str s) -> s
  | _ -> failwith "precondition violated: sov"

let eval_init vl d = eval d (find_lbls vl d)