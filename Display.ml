open Graphics

  (* Light Blue *)
  let color1 = 10288380
  (* Dark Blue *)
  let color2 = 155042
  (* Black *)
  let color3 = 0
  (* Light Grey *)
  let color4 = 13948116
  (* Terminal Green *)
  let color5 = 3407718

  (* Letters for labeling columns *)
  let letters = ["A";"B";"C";"D";"E";"F";"G";"H";"I";"J"]
  (* Numbers for labeling rows *)
  let numbers = ["1";"2";"3";"4";"5";"6";"7";"8";"9";"10"]
  (* Ship names for stats window *)
  let shipnames = ["Carrier";"Battleship";"Cruiser";"Destroyer";"Patrol Boat"]

(* Draws a line from the starting point to the endpoint *)
let drawline (startx, starty) (endx, endy) width color =
  moveto startx starty;
  set_color color;
  set_line_width width;
  lineto endx endy;
  set_color 0;
  set_line_width 1

(* Draws a grid starting from the bottom left x, y coordinates *)
let rec drawgrid (start_x: int) (start_y: int) (sq_width: int)
              (spacing: int) (dim: int) (fill: bool) : unit =

  let rec across x y w s sd f =
    match sd with
    | 0 -> ()
    | z ->
      let curr_x = (x + ((z - 1) * (w + s))) in
      let () =
        if f then
          fill_rect curr_x y w w
        else
          draw_rect curr_x y w w in
      across x y w s (sd - 1) f

  and up x y w s sd cd f=
    match cd with
    | 0 -> ()
    | z ->
      let curr_y = (y + ((z - 1) * (w + s))) in
      let () = across x curr_y w s sd f in
      up x y w s sd (cd - 1) f

  in up start_x start_y sq_width spacing dim dim fill

(* Draws a list of strings horizontally or vertically *)
let rec drawtextlist (start_x: int) (start_y: int) (text: string list)
                    (size: int) (spacing: int) (across: bool) : unit =

  let () = set_text_size size in

  let rec distribute x y txt s c acr =
    match txt with
    | [] -> ()
    | hd::tl ->
      if acr then
        let curr_x = (x + (c * s)) in
        let () = moveto curr_x y in
        let () = draw_string hd in
        distribute x y tl s (c + 1) acr;
      else
        let curr_y = (y - (c * s)) in
        let () = moveto x curr_y in
        let () = draw_string hd in
        distribute x y tl s (c + 1) acr;

  in distribute start_x start_y text spacing 0 across

let open_battleship_window () =
  let () = open_graph " 600x600" in
  set_window_title "Battleship"

let draw_play_board x y =
  set_color color1;
  drawgrid x y 20 5 10 true;
  set_color color2;
  drawgrid x y 20 5 10 false;
  drawtextlist (x + 8) (y + 248) letters 12 25 true;
  drawtextlist (x - 13) (y + 230) numbers 12 25 false

let draw_peg_grid () =
  draw_play_board 35 320

let draw_ship_grid () =
  draw_play_board 335 20

let draw_blind () =
  set_color color4;
  fill_rect 0 0 600 600;
  set_color 0;
  set_text_size 200;
  moveto 256 300;
  draw_string "Click when ready"

let draw_stats () =
  set_color color4;
  fill_rect 320 320 260 260;
  set_color 0;
  set_line_width 3;
  draw_rect 320 320 260 260;
  drawline (320, 530) (580, 530) 3 0;
  drawline (330, 500) (570, 500) 2 0;
  drawline (450, 520) (450, 330) 2 0;
  moveto 335 510;
  draw_string "Player 1's Ships";
  moveto 470 510;
  draw_string "Player 2's Ships";
  moveto 357 550;
  draw_string "It is currently Player _'s turn!";
  drawtextlist 355 465 shipnames 25 25 false;
  drawtextlist 485 465 shipnames 25 25 false

let draw_console () =
  set_color color4;
  fill_rect 20 20 260 260;
  set_color 0;
  set_line_width 3;
  draw_rect 20 20 260 260;
  fill_rect 30 30 240 180;
  fill_rect 30 220 240 50;
  set_line_width 1;
  set_color color5;
  moveto 128 240;
  draw_string "Console:";
  drawtextlist 70 180 ["Random command 1"; "Hello world"] 25 25 false


let draw_game () =
  auto_synchronize false;
  open_battleship_window ();
  draw_peg_grid ();
  draw_ship_grid ();
  drawline (0, 300) (600, 300) 3 0;
  drawline (300, 0) (300, 600) 3 0;
  draw_stats ();
  draw_console ();
  synchronize ()

let () =
  draw_game ()
