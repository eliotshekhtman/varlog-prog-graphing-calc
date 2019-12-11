(** 
   This is the module in which all the evaluation takes place.
*)

open Ast 
open Array
open VarLog

(**[parse s] parses [s] into an AST. *)
val parse: string -> Ast.expr

(**[string_of_val e] converts [e] to a string.
    Requires: [e] is a value. *)
val string_of_val: Ast.value -> string

(** [eval e] fully graphs [e].*)
val eval_graph: Ast.expr -> float -> float

(** [derive x f] is the derivative of function [f] at x-coordinate [x] *)
val derive: float -> (float -> float) -> float

(** [pull_num n] is the number stored in [n]. Requires: [n] has type Num *)
val pull_num: Ast.value -> float

(** [eval_expr vl e] evaluates [e]. Requires: [e] has type expression *)
val eval_expr: (string * Ast.value) list -> Ast.expr -> 
  Ast.value * (string * Ast.value) list

(** [eval_init vl d] is the result of evaluating definition [d],
    tupled to the state at the end of evaluation *)
val eval_init: VarLog.t -> Ast.defn -> Ast.value * VarLog.var list

(** [print_matrix m] returns the string representation of matrix [m] *)
val print_matrix : float array array -> string