(** [entry] represents the calculator user's entry of a
    floating-point number or an operator. *)
type entry =
  | Num of float
  | Add
  | Subtract
  | Multiply
  | Divide
  | Power
  | OpenParen
  | CloseParen

(** [iexpr] is a representation of an infix expression as a
    tree. *)
(* type iexpr =
   | IConst of float
   | IAdd of iexpr * iexpr
   | INeg of iexpr *)

type stack = float list 

(* 5 + 4 * 2 = 5 4 2 * + *)
let pemdas es e =
  let rec helper s es e =
    match es, e with
    | [], _ -> false
    | h :: t, en when en = Add || en = Subtract -> 
      if h = Multiply || h = Divide || h = Power then 
        if Stack.is_empty s then true
        else helper s t e
      else
      if h = OpenParen then (Stack.push OpenParen s; helper s t e)
      else
      if h = CloseParen then 
        try ( if Stack.pop s = OpenParen then helper s t e else helper s t e )
        with _ -> false
      else helper s t e
    | h :: t, en when en = Multiply || en = Divide ->
      if h = Power then
        if Stack.is_empty s then true
        else helper s t e
      else
      if h = OpenParen then (Stack.push OpenParen s; helper s t e)
      else
      if h = CloseParen then 
        try ( if Stack.pop s = OpenParen then helper s t e else helper s t e )
        with _ -> false
      else helper s t e
    | _ -> true
  in helper (Stack.create ()) es e

(** [calc es] is the top number of the stack that results
    from processing the entries in [es].  [None] if the 
    resulting stack is empty. *)
let calc es = 
  let rec iter s st e = 
    match e, st with 
    | Num f :: t,_ -> iter s (f :: st) t
    | Add :: t, x :: y :: rest -> 
      if pemdas e Add then 
        match iter s (y :: rest) t with 
        | [] -> []
        | y :: rest -> iter s (x +. y :: rest) []
      else
        iter s (x +. y :: rest) t
    | Subtract :: t, x :: y :: rest -> 
      if pemdas e Subtract then 
        match iter s (y :: rest) t with 
        | [] -> []
        | y :: rest -> iter s (x -. y :: rest) []
      else
        iter s (x -. y :: rest) t
    | Multiply :: t, x :: y :: rest -> iter s (x *. y :: rest) t
    | Divide :: t, x :: y :: rest -> iter s (x /. y :: rest) t
    | _ -> st
  in
  match iter (Stack.create ()) [] es with
  | [] -> None
  | x :: _ -> Some x

(** [rpn_of_iexpr iexp] is the RPN representation of an 
    expression tree. *)
(* let rec rpn_of_iexpr = function
   | IConst f -> [Num f]
   | IAdd (e1, e2) -> rpn_of_iexpr e1 @ rpn_of_iexpr e2 @ [Plus]
   | INeg e -> rpn_of_iexpr e @ [Negate] *)

let split_string input =
  let rec clean_list = function 
    | [] -> []
    | Str.Delim s :: t -> s :: clean_list t
    | Str.Text s :: t -> s :: clean_list t
  in
  let rec remove_whitespace = function
    | [] -> []
    | " " :: t -> remove_whitespace t 
    | "" :: t -> remove_whitespace t 
    | h :: t -> h :: remove_whitespace t 
  in Str.full_split (Str.regexp " ?*?/?+?-?\^?\(?\)?") input 
     |> clean_list |> remove_whitespace

let parse input =
  let rec helper broken =
    match broken with 
    | [] -> []
    | "+" :: t -> (helper t) @ [Add]
    | "*" :: t -> (helper t) @ [Multiply]
    | "-" :: t -> (helper t) @ [Subtract]
    | "/" :: t -> (helper t) @ [Divide]
    | "^" :: t -> (helper t) @ [Power]
    | "(" :: t -> OpenParen :: helper t (* have to put after all operators *)
    | ")" :: t -> (helper t) @ [CloseParen]
    | h :: t -> 
      if Str.string_match (Str.regexp "[0-9][0-9]*\(\.?\)[0-9]*") h 0 then (* make helper to split smarter *)
        (Num (Float.of_string (if String.contains h '.' then h else h ^ "."))) :: (helper t)
      else helper t
  in input |> split_string |> helper

let rec read () =
  print_string "> ";
  match read_line () |> parse |> calc with
  | None -> print_endline "Empty"
  | Some f -> f |> Float.to_string |> print_endline; read ()