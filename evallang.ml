open Ast 
open Evalexpr 

module VarLog = struct
  type var = string * value
  type t = ((var list) * ((string * defn) list)) ref

  let empty () : t = ref ([], [])

  let find id vl = 
    try Some (List.assoc id (fst !vl)) with _ -> None

  let bind id v vl = 
    let rec helper = function 
      | [] -> [(id, v)]
      | h :: t -> 
        if (fst h) = id then (id, v) :: t 
        else h :: helper t
    in vl := helper (fst !vl), snd !vl

  let expose vl = fst !vl

  let bind_lbl id d vl = 
    let rec helper = function 
      | [] -> [(id, d)] 
      | h :: t -> 
        if (fst h) = id then failwith "parsing error: 2 lbls of same name"
        else h :: helper t 
    in vl := fst !vl, helper (snd !vl)

  let find_lbl id vl = List.assoc id (snd !vl)
end

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
  | DOutput (_, _, _, d) -> has_goto d
  | DLine (_, _, _, _, d) -> has_goto d
  | DFunction (_, _, d) -> has_goto d
  | DDefStruct (_, _, _, d) -> has_goto d
  | DInstantiateStruct (_, _, _, d) -> has_goto d
  | DStructSet (_, _, _, d) -> has_goto d
  | _ -> failwith "Unimplemented: has_goto"

let rec eval d vl = 
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
  | DOutput (x, y, v, d) -> eval_output x y v d vl
  | DLine (x1, y1, x2, y2, d) -> eval_line x1 y1 x2 y2 d vl
  | DFunction (name, args, body) -> 
    VarLog.bind name (eval_func args body vl) vl;
    (Null, VarLog.expose vl)
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
      if List.length cargs != List.length xe then failwith "precondition violated: wrong # of args in constructor"
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

and eval_output x y v d vl = 
  let x' = x |> eval_expr (VarLog.expose vl) |> fst in 
  let y' = y |> eval_expr (VarLog.expose vl) |> fst in 
  let v' = v |> eval_expr (VarLog.expose vl) |> fst in 
  match x', y' with 
  | Num a, Num b -> begin 
      Graphing.output a b (v' |> string_of_val); eval d vl
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

(** [string_of_val e] converts [e] to a string.
    Requires: [e] is a value. *)
let string_of_val (e : expr) : string =
  match e with
  | Val (Num f) -> string_of_float f
  | Val (Bool b) -> string_of_bool b
  | Val (Str s) -> s
  | _ -> failwith "precondition violated: sov"

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
  | DOutput (_, _, _, d) -> find_lbls vl d
  | DLine (_, _, _, _, d) -> find_lbls vl d
  | DFunction (_, _, d) -> find_lbls vl d
  | DDefStruct (_, _, _, d) -> find_lbls vl d
  | DInstantiateStruct (_, _, _, d) -> find_lbls vl d
  | DStructSet (_, _, _, d) -> find_lbls vl d
  | DWhile (_, _, d) -> find_lbls vl d
  | _ -> failwith "Unimplemented: find_lbls"

let eval_init vl d = eval d (find_lbls vl d)