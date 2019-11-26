(**[better_int_of_float f] converts float [f] into an int*)
val better_int_of_float: float -> int

(**[open_output_window] opens a graphing window*)
val open_output_window: unit -> unit

(**[output x y s] prints string s at x,y position [x],[y]*)
val output: float -> float -> string -> unit

(** [graph_func xi xa yi ya x f] is [unit], but it graphs function f, 
    starting at pixel [x], with bounds min x = [xi], max x = [xa],
    min y = [yi], max y = [ya]. *)
val graph_func: float -> float -> float -> float -> 'a -> (float -> float) ->
  unit