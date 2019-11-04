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



let _ = resize_window 800 800




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


let _ = moveto 0 400
let _ = Graphics.lineto 800 400
(* let _ = moveto 2 130 *)
(* let _ = draw_string_v "abscissa" *)
let _ = moveto 400 0
let _ = Graphics.lineto 400 800
(* let _ = moveto 135 280  *)
(* let _ = draw_string_v "ordinate" *)

(* 
let scale ((a,b,c,d) : (int * int * int * int)) : unit = 
  match (a,b,c,d) with 
  | a -> begin 
     *)