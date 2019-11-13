open Ast

module type VL = sig 
  type var 
  type t
  val empty : t
  val find : string -> t -> value option
  val bind : string -> value -> t -> unit
end

module VarLog : VL = struct
  type var = string * value
  type t = var list ref

  let empty : t = ref []

  let find id vl = 
    try Some (List.assoc id (!vl)) with _ -> None

  let bind id v vl = 
    let rec helper = function 
      | [] -> [(id, v)]
      | h :: t -> 
        if (fst h) = id then (id, v) :: t 
        else h :: helper t
    in vl := helper !vl

end

let substitute vv vl =
  match vv with 
  | Val v -> v
  | Var v -> begin 
      match VarLog.find v vl with 
      | None -> failwith "precondition violated: no binding"
      | Some v -> v
    end
  | _ -> failwith "precondition violated: not variable or value"

let get_val_num vv vl =
  match substitute vv vl with 
  | Num i -> i 
  | _ -> failwith "precondition violated: not number"

let rec fact n = if n = 0. then 1. else n *. fact (n -. 1.)

(** [is_value e] is whether [e] is a value. *)
let is_value : expr -> bool = function
  | Val _ -> true
  | Var _ -> true
  | _ -> false

(** [step e] takes a single step of evaluation of [e]. *)
let rec step vl = function
  | Val _ -> failwith "Does not step"
  | Var v -> Val (substitute (Var v) vl)
  | Binop (bop, e1, e2) when is_value e1 && is_value e2 -> 
    step_bop vl bop e1 e2
  | Binop (bop, e1, e2) when is_value e1 ->
    Binop (bop, e1, step vl e2)
  | Binop (bop, e1, e2) -> Binop (bop, step vl e1, e2)
  | Uniop (uop, e) when is_value e ->
    step_uop vl uop e
  | Uniop (uop, e) -> Uniop (uop, step vl e)
  | Bind (s, e1, e2) when is_value e1 && is_value e2 ->
    VarLog.bind s (substitute e1 vl) vl; Val (substitute e2 vl)
  | Bind (s, e1, e2) when is_value e1 -> 
    VarLog.bind s (substitute e1 vl) vl; step vl e2
  | Bind (s, e1, e2) -> Bind (s, step vl e1, e2)
  | _ -> failwith "fuck u"

(** [step_bop bop v1 v2] implements the primitive operation
    [v1 bop v2].  Requires: [v1] and [v2] are both values. *)
and step_bop vl bop e1 e2 = match bop, e1, e2 with
  | Add, a, b -> Val (Num ((get_val_num a vl) +. (get_val_num b vl)))
  | Mult, a, b -> Val (Num ((get_val_num a vl) *. (get_val_num b vl)))
  | Subt, a, b -> Val (Num ((get_val_num a vl) -. (get_val_num b vl)))
  | Div, a, b -> Val (Num ((get_val_num a vl) /. (get_val_num b vl)))
  | Pow, a, b -> Val (Num ((get_val_num a vl) ** (get_val_num b vl)))
  | _ -> failwith "precondition violated: bop"

and step_uop vl uop e = match uop, e with
  | Fact, a -> Val (Num (fact (get_val_num a vl)))
  | _ -> failwith "precondition violated: uop"


(** [eval e] fully evaluates [e] to a value. *)
let rec eval vl e = 
  if is_value e then e
  else e |> step vl |> eval vl

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
  | Val (Bool i) -> string_of_bool i
  | _ -> failwith "precondition violated: sov"

(** [interp s] interprets [s] by lexing and parsing it, 
    evaluating it, and converting the result to a string. *)
let interp (s : string) : string =
  s |> parse |> eval VarLog.empty |> string_of_val
