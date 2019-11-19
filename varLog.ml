open Ast

module VarLog = struct
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