open Ast 
open Array
open VarLog

(**[fact n] is the factorial of float [n]*)
val fact: float -> float

(** [replace lst k v] is [lst] with [(k, v)] inserted, and if 
    [lst] already had a [(k, _)] tuple, it is replaced with 
    [(k, v)] *)
val replace : (string * value) list -> string -> value -> (string * value) list

(**[is_value e] is whether expr [e] is a valid value*)
val is_value: Ast.expr -> bool

(**[parse s] parses [s] into an AST. *)
val parse: string -> Ast.expr

(**[is_int f] is whether a float [f] is a float representation of an integer*)
val is_int: float -> bool

(**[print_matrix m] prints matrix [m]*)
val print_matrix: float array array -> string

(**[scalar_mult f m] is the result of scalar multiplication of float [f] with matrix [m]*)
val scalar_mult: float -> float array array -> float array array

(**[matrix_mult m1 m2] is the product of matrix [m1] and matrix [m2]*)
val matrix_mult: float array array -> float array array -> float array array

(**[string_of_val e] converts [e] to a string.
    Requires: [e] is a value. *)
val string_of_val: Ast.value -> string

(**[is_value e] is whether [e] is a value. *)
val is_value_graph: Ast.expr -> bool

(**[get_val v] is the number stored in value [v] *)
val get_val: float -> Ast.expr -> float

(** [step e] takes a single step in the graphing of [e]*)
val step_graph: float -> Ast.expr -> Ast.expr

(** [step_bop bop v1 v2] implements the primitive operation
    [v1 bop v2].  Requires: [v1] and [v2] are both values. *)
val step_bop: float -> Ast.bop -> Ast.expr -> Ast.expr -> Ast.expr

(** [step_bop uop v] implements the primitive operation
    [uop v].  Requires: [v] is a value. *)
val step_uop: float -> Ast.uop -> Ast.expr -> Ast.expr

(** [eval e] fully graphs [e].*)
val eval_graph: Ast.expr -> float -> float

(**[integrate b1 b2 acc f] is the definite integral of function [f] between
   bounds [b1] and [b2]*)
val integrate: float -> float -> float -> (float -> float) -> float

(**[derive x f] is the derivative of function [f] at x-coordinate [x]*)
val derive: float -> (float -> float) -> float

(**[add_helper v1 v2] is the sum of [v1] and [v2].
   Requires: [v1] and [v2] are both values.*)
val add_helper: Ast.value -> Ast.value -> Ast.value

(**[add_helper v1 v2] is whether [v1] and [v2] are equal.
   Requires: [v1] and [v2] are both values.*)
val eq_helper: Ast.value -> Ast.value -> Ast.value

(**[add_helper v1 v2] is whether [v1] is less than [v2].
   Requires: [v1] and [v2] are both values.*)
val lt_helper: Ast.value -> Ast.value -> Ast.value

(**[add_helper v1 v2] is whether [v1] is greater than [v2].
   Requires: [v1] and [v2] are both values.*)
val gt_helper: Ast.value -> Ast.value -> Ast.value

(**[add_helper v1 v2] is the "not" operator acted on v1.
   Requires: [v1] is of type Bool*)
val not_val: Ast.value -> Ast.value

(**[substitute vl e] is the value that [e] represents in the
   association list vl. Requires: [e] is a string*)
val substitute: (string * 'a) list -> string -> 'a

(**[pull_num n] is the number stored in n. Requires: [n] has type Num*)
val pull_num: Ast.value -> float

(**[eval_expr vl e] evaluates [e]. Requires: [e] has type expression*)
val eval_expr: (string * Ast.value) list -> Ast.expr -> 
  Ast.value * (string * Ast.value) list

(**[eval_randint vl a b] *)
val eval_randint: (string * Ast.value) list -> Ast.expr -> Ast.expr -> 
  Ast.value * (string * Ast.value) list

val eval_matrix: Ast.expr -> Ast.expr -> (string * Ast.value) list -> 
  Ast.value * (string * Ast.value) list

val eval_matrixget: (string * Ast.value) list -> Ast.expr -> Ast.expr -> 
  Ast.expr -> Ast.value * (string * Ast.value) list

val eval_boop: (string * Ast.value) list -> Ast.boop -> Ast.expr -> Ast.expr -> 
  Ast.value * (string * Ast.value) list

(**[eval_expr vl bop e1 e2] evaluates the primitive expression [e1] [bop] [e2].
   Requires: [e] has type expression*)
val eval_bop: (string * Ast.value) list -> Ast.bop -> Ast.expr -> Ast.expr -> 
  Ast.value * (string * Ast.value) list

(**[eval_expr vl uop e] evaluates the primitive expression [uop] [e].
   Requires: [e] has type expression*)
val eval_uop: (string * Ast.value) list -> Ast.uop -> Ast.expr -> 
  Ast.value * (string * Ast.value) list

(**[eval_expr vl top e1 e2 e3] evaluates the primitive expression [top] [e1] 
   [e2] [e3]. Requires: [e] has type expression*)
val eval_top: (string * Ast.value) list -> Ast.top -> Ast.expr -> Ast.expr -> 
  Ast.expr -> Ast.value * (string * Ast.value) list

(**[eval_deriv vl der e1 e2] evaluates the derivative der*)
val eval_deriv: (string * Ast.value) list -> Ast.der -> Ast.expr -> Ast.expr -> 
  Ast.value * (string * Ast.value) list

(* Definitions! *)

val has_goto: Ast.defn -> bool

val eval: Ast.defn -> ((string * Ast.value) list * (string * Ast.defn) list) ref
  -> Ast.value * VarLog.var list

val eval_output: Ast.expr -> Ast.expr -> Ast.expr -> Ast.expr -> Ast.defn -> 
  ((string * Ast.value) list * (string * Ast.defn) list) ref -> Ast.value * VarLog.var list

val eval_matrixset: Ast.expr -> Ast.expr -> Ast.expr -> Ast.expr -> Ast.defn ->
  ((string * Ast.value) list * (string * Ast.defn) list) ref -> Ast.value * VarLog.var list

val eval_disp: Ast.expr -> Ast.defn ->
  ((string * Ast.value) list * (string * Ast.defn) list) ref -> Ast.value * VarLog.var list

val eval_prompt: string -> Ast.defn ->
  ((string * Ast.value) list * (string * Ast.defn) list) ref -> Ast.value * VarLog.var list

val eval_assign: string -> Ast.expr -> Ast.defn ->
  ((string * Ast.value) list * (string * Ast.defn) list) ref -> Ast.value * VarLog.var list

val eval_if: Ast.expr -> Ast.defn -> Ast.defn -> Ast.defn ->
  ((string * Ast.value) list * (string * Ast.defn) list) ref -> Ast.value * VarLog.var list

val string_of_expr : Ast.expr -> string

val find_lbls: ('a * (string * Ast.defn) list) ref -> Ast.defn -> 
  ('a * (string * Ast.defn) list) ref

val eval_init: VarLog.t -> Ast.defn -> Ast.value * VarLog.var list