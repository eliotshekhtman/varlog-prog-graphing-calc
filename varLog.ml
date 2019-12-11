(** 
   VarLog is the module in which the label and variable environments are stored
   and used in all the other modules for input/output and evaluation.
*)
open Ast

module type VL = sig 
  (** type [var] is the name of a variable and the value
      it holds *)
  type var = string * value

  (** type [t] is the type of a VarLog; the first half of the 
      ref holds all the variables and their bindings, and the
      second half holds all the labels and the definitions that
      come after them *)
  type t = ((var list) * ((string * defn) list)) ref

  (**[empty ()] is the empty [t] representation *)
  val empty : unit -> t

  (**[find id vl] is the value associated with a certain [id] in
     the [vl] association list *)
  val find : 'a -> (('a * 'b) list * 'c) ref -> 'b option

  (**[bind id v vl] is [unit], binds Value [v] to identifier [id] 
     in VarLog [vl]*)
  val bind : 'a -> 'b -> (('a * 'b) list * 'c) ref -> unit


  (**[expose vl] is the association list of variables stored in VarLog [vl]*)
  val expose : ('a * 'b) ref -> 'a


  (**[find_lbl id vl] is the association list containing labels given
     certain [id], which is the name for the label*)
  val bind_lbl : 'a -> 'b -> ('c * ('a * 'b) list) ref -> unit

  (**[bind_lbl id d vl] binds label [d] to identifier [id] in VarLog [vl]*)

  val find_lbl : 'a -> ('b * ('a * 'c) list) ref -> 'c

end

(** [VarLog] is the module that contains the bindings for both 
    variables and labels, and the functions pertaining to them. *)
module VarLog : VL = struct
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
