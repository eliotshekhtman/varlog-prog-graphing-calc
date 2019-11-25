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
  | DReturn d -> has_goto d
  | DDisp (_, d) -> has_goto d
  | DAssign (_, _, d) -> has_goto d
  | DIf (_, _, _, d) -> has_goto d
  | DGoto (_, d) -> true 
  | DGotoSub (_, d) -> has_goto d
  | DLabel (_, d) -> has_goto d
  | DMatrixSet (_, _, _, _, d) -> has_goto d
  | DOutput (_, _, _, d) -> has_goto d
  | _ -> failwith "Unimplemented"

let rec eval d vl = 
  match d with 
  | DEnd -> ()
  | DReturn d -> ()
  | DDisp (e, d) -> eval_disp e d vl 
  | DAssign (s, e, d) -> eval_assign s e d vl
  | DIf (e, d1, d2, d3) -> eval_if e d1 d2 d3 vl
  | DLabel (s, d) -> eval d vl 
  | DGoto (s, d) -> eval (VarLog.find_lbl s vl) vl
  | DGotoSub (s, d) -> eval (VarLog.find_lbl s vl) vl; eval d vl
  | DMatrixSet (m, e1, e2, e3, d) -> eval_matrixset m e1 e2 e3 d vl
  | DOutput (x, y, v, d) -> eval_output x y v d vl
  | _ -> failwith "Unimplemented"

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
  | _ -> failwith "precondition violated: matrix set"

and eval_disp e d vl = 
  let v = e |> eval_expr (VarLog.expose vl) |> fst |> string_of_val in 
  print_endline v;
  eval d vl
and eval_assign s e d vl =
  let v = e |> eval_expr (VarLog.expose vl) |> fst in 
  VarLog.bind s v vl; eval d vl
and eval_if e d1 d2 d3 vl =
  match e |> eval_expr (VarLog.expose vl) |> fst with 
  | Bool true -> eval d1 vl; if(d1 |> has_goto |> not) then eval d3 vl
  | Bool false -> eval d2 vl; if(d2 |> has_goto |> not) then eval d3 vl
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
  | DReturn d -> find_lbls vl d
  | DDisp (_, d) -> find_lbls vl d 
  | DAssign (_, _, d) -> find_lbls vl d 
  | DIf (_, _, _, d) -> find_lbls vl d
  | DGoto (_, d) -> find_lbls vl d 
  | DGotoSub (_, d) -> find_lbls vl d
  | DLabel (s, d) -> VarLog.bind_lbl s d vl; find_lbls vl d
  | DMatrixSet (_, _, _, _, d) -> find_lbls vl d
  | DOutput (_, _, _, d) -> find_lbls vl d
  | _ -> failwith "Unimplemented"

let eval_init d = eval d (find_lbls (VarLog.empty ()) d)