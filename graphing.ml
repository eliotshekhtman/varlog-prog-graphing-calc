open Graphics

(** [better_int_of_float f] is [f] cast to an integer, for the purposes
    of the graph: if it's too big or too small, set to the extrema of [int],
    divided by 2, because we'll never set the graph to be that large 
    anyways. *)
let better_int_of_float (f : float) = 
  if f >= (max_int |> float_of_int) then max_int / 2
  else if f <= (min_int |> float_of_int) then min_int / 2
  else f |> int_of_float

let window_height = 800 
let window_width = 800 

let open_output_window () = 
  open_graph "";
  set_window_title "Output";
  resize_window window_width window_height;
  set_color black;
  set_text_size 20;
  ()

let output x y s = 
  let incx = (window_width / 30) |> float_of_int in 
  let incy = (window_height / 30) |> float_of_int in 
  let x' = (x *. incx) |> int_of_float in 
  let y' = window_height - (((y +. 1.) *. incy) |> int_of_float) in
  moveto x' y';
  draw_string s;
  ()

(** [graph_func xi xa yi ya x f] is [unit], but it graphs function f, 
    starting at pixel [x], with bounds min x = [xi], max x = [xa],
    min y = [yi], max y = [ya]. *)
let rec graph_func xi xa yi ya x f =
  (* Set up window *)
  let _ = open_graph "" in
  let _ = set_window_title "Graph 2-D Functions" in
  let _ = resize_window window_width window_height in
  let num_ticks = 10 in
  set_color black;

  let tick_height = window_height / num_ticks in
  let tick_width = window_width / num_ticks in

  (* Set bounds for graph *)
  let x_min = xi in
  let x_max = xa in
  let y_min = yi in
  let y_max = ya in

  let pixel_width = 
    (x_max -. x_min) /. (float_of_int window_width) in
  let pixel_height = 
    (y_max -. y_min) /. (float_of_int window_height) in

  let xpix_to_coord x = x_min +. pixel_width *. x in
  let ypix_to_coord y = y_min +. pixel_height *. y in
  let ycoord_to_pix y = (y -. y_min) /. pixel_height in

  (* Create horizontal axis *)
  let _ = moveto 0 (window_height / 2) in
  let _ = Graphics.lineto window_width (window_height / 2) in

  (* Create vertical axis *)
  let _ = moveto (window_width / 2) 0 in
  let _ = Graphics.lineto (window_width / 2) window_height in

  (* Create tickmarks *)
  let tick_inc = 10 in
  let rec vert_ticks c h = 
    if c <> 0 then begin
      moveto (window_width / 2 - tick_inc) h;
      lineto (window_width / 2 + tick_inc) h;
      moveto (window_width / 2 + 2 * tick_inc) h;
      h |> float_of_int |> ypix_to_coord |> string_of_float |> draw_string;
      vert_ticks (c-1) (h + tick_height)
    end
    else () in 
  let rec horz_ticks c w = 
    if c <> 0 then begin
      moveto w (window_height / 2 - tick_inc);
      lineto w (window_height / 2 + tick_inc);
      moveto w (window_height / 2 + 2 * tick_inc);
      w |> float_of_int |> ypix_to_coord |> string_of_float |> draw_string;
      horz_ticks (c-1) (w + tick_width)
    end
    else () in
  vert_ticks num_ticks 0; horz_ticks num_ticks 0;

  (* Set up for the graphing of the function *)
  let _ = moveto 0 (ycoord_to_pix (0. |> f |> xpix_to_coord) |> int_of_float) 
  in
  set_color blue;
  let rec helper x = 
    if x >= window_width then ()
    else begin
      let y_pix = x |> float_of_int |> xpix_to_coord |> f 
                  |> ycoord_to_pix |> better_int_of_float in
      (* print_string 
         ((x |> float_of_int |> xpix_to_coord |> f |> string_of_float) 
         ^ " "); *)
      (* print_endline (current_y () |> string_of_int); *)
      if current_y () <= 0 && y_pix < 0 then Graphics.moveto x 0
      else if y_pix < 0 then Graphics.lineto x 0
      else if current_y () >= window_height && y_pix > window_height then 
        Graphics.moveto x window_height
      else if y_pix > window_height then Graphics.lineto x window_height
      else Graphics.lineto x y_pix; 
      helper (x+1)
    end
  in helper 0

(* let _ = graph_func 0 func *)