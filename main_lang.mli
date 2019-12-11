open Ast 

(** [parse s] is the defn resulting from [s] *)
val parse : string -> Ast.defn

(** [interp s] is the error displayed after attempting to run
    the file represented by string [s], but also enables the 
    cohesive running of the file, with all the side-effects
    entailed *)
val interp: string -> string