open Ast
open Graphics

let better_int_of_float (f : float) = 
  if f >= (max_int |> float_of_int) then max_int / 2
  else if f <= (min_int |> float_of_int) then min_int / 2
  else f |> int_of_float

let window_height = 800 
let window_width = 800 
let num_ticks = 10
let tick_height = window_height / num_ticks
let tick_width = window_width / num_ticks


let open_output_window () = 
  open_graph "";
  set_window_title "Output";
  resize_window window_width window_height;
  set_color black;
  set_text_size 20;
  ()

(**[parse_color color] converts an Ast.Color to a form recognizable by the
   OCaml graphing module.*)
let parse_color = function 
  | Color Red -> red 
  | Color Black -> black 
  | Color Green -> green 
  | Color Yellow -> yellow 
  | Color Blue -> blue
  | _ -> failwith "precondition violated: not a color"

let output x y s c = 
  let incx = window_width / 30 in 
  let incy = window_height / 30 in 
  let x' = (x *. (incx |> float_of_int)) |> int_of_float in 
  let y' = window_height - (((y +. 1.) *. (incy |> float_of_int)) 
                            |> int_of_float) in
  let (x_len, _) = text_size s in
  let offset = 
    if x_len > incx then 0 
    else (incx - x_len) / 2 in
  moveto (x' + offset) y';
  set_color white;
  let ix = incx - 1 in
  let iy = incx - 1 in 
  fill_rect x' y' ix iy;
  c |> parse_color |> set_color;
  draw_string s;
  set_color black;
  ()

let draw_line x1 y1 x2 y2 = 
  let xi1 = int_of_float x1 in 
  let yi1 = int_of_float y1 in 
  let xi2 = int_of_float x2 in 
  let yi2 = int_of_float y2 in 
  let x_interval = window_width / 30 in 
  let y_interval = window_height / 30 in 
  moveto (xi1 * x_interval) (window_height - yi1 * y_interval);
  lineto (xi2 * x_interval) (window_height - yi2 * y_interval); ()

(**[graph_setup x_min x_max y_min y_max] creates a new empty graphing window
   with range [x_min] to [x_max] and domain [y_min] to [y_max].*)
let graph_setup x_min x_max y_min y_max = 
  (* Set up window *)
  open_graph "";
  set_window_title "Graph 2-D Functions";
  resize_window window_width window_height;
  set_color black;
  let pixel_height = (y_max -. y_min) /. (float_of_int window_height) in
  let ypix_to_coord y = y_min +. pixel_height *. y in

  (* Create horizontal axis *)
  moveto 0 (window_height / 2);
  Graphics.lineto window_width (window_height / 2);

  (* Create vertical axis *)
  moveto (window_width / 2) 0;
  Graphics.lineto (window_width / 2) window_height;

  (* Create tickmarks *)
  let tick_inc = 10 in

  (** [vert_ticks c h] creates the vertical tick marks *)
  let rec vert_ticks c h = 
    if c <> 0 then begin
      moveto (window_width / 2 - tick_inc) h;
      lineto (window_width / 2 + tick_inc) h;
      moveto (window_width / 2 + 2 * tick_inc) h;
      h |> float_of_int |> ypix_to_coord |> string_of_float |> draw_string;
      vert_ticks (c-1) (h + tick_height)
    end
    else () in 
  (** [horz_ticks c h] creates the horizontal tick marks *)
  let rec horz_ticks c w = 
    if c <> 0 then begin
      moveto w (window_height / 2 - tick_inc);
      lineto w (window_height / 2 + tick_inc);
      moveto w (window_height / 2 + 2 * tick_inc);
      w |> float_of_int |> ypix_to_coord |> string_of_float |> draw_string;
      horz_ticks (c-1) (w + tick_width)
    end
    else () in

  vert_ticks num_ticks 0; horz_ticks num_ticks 0

let rec graph_func x_min x_max y_min y_max x c f =
  let pixel_width = 
    (x_max -. x_min) /. (float_of_int window_width) in
  let pixel_height = 
    (y_max -. y_min) /. (float_of_int window_height) in

  let xpix_to_coord x = x_min +. pixel_width *. x in
  let ycoord_to_pix y = (y -. y_min) /. pixel_height in

  (* Set up for the graphing of the function *)
  moveto 0 (ycoord_to_pix (0. |> f |> xpix_to_coord) |> int_of_float);
  set_color c;
  let rec helper x = 
    if x >= window_width then ()
    else begin
      let y_pix = x |> float_of_int |> xpix_to_coord |> f 
                  |> ycoord_to_pix |> better_int_of_float in
      if current_y () <= 0 && y_pix < 0 then Graphics.moveto x 0
      else if y_pix < 0 then Graphics.lineto x 0
      else if current_y () >= window_height && y_pix > window_height then 
        Graphics.moveto x window_height
      else if y_pix > window_height then Graphics.lineto x window_height
      else Graphics.lineto x y_pix; 
      helper (x+1)
    end
  in helper x

let graph xi xa yi ya f = 
  graph_setup xi xa yi ya;
  graph_func xi xa yi ya 0 blue f