(**[better_int_of_float f] converts float [f] into an int*)
val better_int_of_float: float -> int

(**[open_output_window] opens a graphing window*)
val open_output_window: unit -> unit

(**[output x y s] prints string s at x,y position [x],[y]*)
val output: float -> float -> string -> unit

(** [graph_func xi xa yi ya x c f] is [unit], but it graphs 
    function [f], with bounds min x = [xi], max x = [xa], 
    min y = [yi], and max y = [ya], starting at [x] and with 
    color [c] *)
val graph_func : float -> float -> float -> float -> int -> 
  Graphics.color -> (float -> float) -> unit

(** [graph xi xa yi ya f] is [unit], but it graphs function [f], 
    with bounds min x = [xi], max x = [xa],
    min y = [yi], max y = [ya]. *)
val graph: float -> float -> float -> float -> (float -> float) ->
  unit