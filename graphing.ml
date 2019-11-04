open Graphics


let _ = open_graph ""
let _ = set_window_title "Graph 2-D Functions"
(* let _ = size_x ()
   let _ = size_y () *)
(* let _ = plot 100 100
   let _ = plot 101 101
   let _ = plot 102 102
   let _ = plot 103 103
   let _ = plot 104 104
   let _ = plot 105 105
   let _ = plot 106 106
   let _ = plot 107 107 *)

let window_height = 800
let window_width = 800

let x_min = 1.
let x_max = 2.
let y_min = 1.
let y_max = 2.

let pixel_width = 
  (x_max -. x_min) /. (float_of_int window_width)
let pixel_height = 
  (y_max -. y_min) /. (float_of_int window_height)

let _ = resize_window window_width window_height




let linear a b inputs = b

(* 
let draw_string_v s =

  let (xi,yi) = Graphics.current_point()
  and l = String.length s
  and (_,h) = Graphics.text_size s
  in
  Graphics.draw_char s.[0];
  for i=1 to l-1 do
    let (_,b) = Graphics.current_point()
    in Graphics.moveto xi (b-h);
    Graphics.draw_char s.[i]
  done;
  let (a,_) = Graphics.current_point() in Graphics.moveto a yi *)


let _ = moveto 0 (window_height / 2)
let _ = Graphics.lineto window_width (window_height / 2)
(* let _ = moveto 2 130 *)
(* let _ = draw_string_v "abscissa" *)
let _ = moveto (window_width / 2) 0
let _ = Graphics.lineto (window_width / 2) window_height
(* let _ = moveto 135 280  *)
(* let _ = draw_string_v "ordinate" *)

(* 
let scale ((a,b,c,d) : (int * int * int * int)) : unit = 
  match (a,b,c,d) with 
  | a -> begin 
     *)

(** [xpix_to_coord x] is the coordinate value for the pixel [x]
    pixels away from the left bound of the graph. *)
let xpix_to_coord x = x_min +. pixel_width *. x

let ycoord_to_pix y = (y -. y_min) /. pixel_height

let func x = x ** 2.

let _ = moveto 0 (ycoord_to_pix (0. |> func |> xpix_to_coord) |> int_of_float)

let rec graph_func x f =
  if x >= window_width then () 
  else begin
    Graphics.lineto 
      x 
      (x 
       |> float_of_int 
       |> xpix_to_coord 
       |> f
       |> ycoord_to_pix 
       |> int_of_float); 
    graph_func (x+1) f
  end

let _ = graph_func 0 func