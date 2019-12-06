open Ast


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
