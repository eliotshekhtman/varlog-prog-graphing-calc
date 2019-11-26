open Ast 
open Evalexpr

module VarLog : sig 

  type var = string * value

  type t = ((var list) * ((string * defn) list)) ref

  val empty: unit -> t

  val find: 'a -> (('a * 'b) list * 'c) ref -> 'b option

  val bind: 'a -> 'b -> (('a * 'b) list * 'c) ref -> unit

  val expose: ('a * 'b) ref -> 'a

  val bind_lbl: 'a -> 'b -> ('c * ('a * 'b) list) ref -> unit

  val find_lbl: 'a -> ('b * ('a * 'c) list) ref -> 'c

end

val has_goto: Ast.defn -> bool

val eval: Ast.defn -> ((string * Ast.value) list * (string * Ast.defn) list) ref
  -> unit

val eval_output: Ast.expr -> Ast.expr -> Ast.expr -> Ast.defn -> 
  ((string * Ast.value) list * (string * Ast.defn) list) ref -> unit

val eval_matrixset: Ast.expr -> Ast.expr -> Ast.expr -> Ast.expr -> Ast.defn ->
  ((string * Ast.value) list * (string * Ast.defn) list) ref -> unit

val eval_disp: Ast.expr -> Ast.defn ->
  ((string * Ast.value) list * (string * Ast.defn) list) ref -> unit

val eval_prompt: string -> Ast.defn ->
  ((string * Ast.value) list * (string * Ast.defn) list) ref -> unit

val eval_assign: string -> Ast.expr -> Ast.defn ->
  ((string * Ast.value) list * (string * Ast.defn) list) ref -> unit

val eval_if: Ast.expr -> Ast.defn -> Ast.defn -> Ast.defn ->
  ((string * Ast.value) list * (string * Ast.defn) list) ref -> unit

val string_of_val: Ast.expr -> string

val find_lbls: ('a * (string * Ast.defn) list) ref -> Ast.defn -> 
  ('a * (string * Ast.defn) list) ref

val eval_init: Ast.defn -> unit