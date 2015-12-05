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
  (* Dark Grey *)
  let color6 = 9145997
  (* Very Light Grey *)
  let color7 = 16053492
  (* Mid Lavender *)
  let color8 = 10790081
  (* Light Lavender *)
  let color9 = 14803434
  (* Dark Lavender *)
  let color10 = 6184584
  (* Mid Peony *)
  let color11 = 13392986
  (* Light Peony *)
  let color12 = 14786462
  (* Dark Peony *)
  let color13 = 8924202
  (* White *)
  let color14 = 16777215
  (* Red *)
  let color15 = 16721408

  (* Letters for labeling columns *)
  let letters = ["A";"B";"C";"D";"E";"F";"G";"H";"I";"J"]
  (* Numbers for labeling rows *)
  let numbers = ["1";"2";"3";"4";"5";"6";"7";"8";"9";"10"]
  (* Ship names for stats window *)
  let shipnames = ["Carrier";"Battleship";"Cruiser";"Destroyer";"Patrol Boat"]

(* Checks one DIM x DIM grid for the position of the mouse *)
let checkgrid (mouse_x: int) (mouse_y: int) (start_x: int)
                (start_y: int) (sq_width: int) (spacing: int)
                (dim: int) : (string * int) option =
  let rec across mx my sx sy w s n_start n_fin : string option =
  (
    match (n_start < n_fin) with
    | false -> None
    | true ->
      if (mx >= (sx + ((n_start * (w + s))))) &&
        (mx <= (sx + (((n_start + 1) * w) + (n_start * s)))) &&
        (my >= sy) &&
        (my <= (sy + w)) then
        Some (List.nth letters n_start)
      else
        across mx my sx sy w s (n_start + 1) n_fin
  )

  and down mx my sx sy w s n_start n_fin : (string * int) option =
  (
    match (n_start < n_fin) with
    | false -> None
    | true ->
      let curr_y = (sy + (n_start * (w + s))) in
      (
        match (across mx my sx curr_y w s 0 n_fin) with
        | None -> down mx my sx sy w s (n_start + 1) n_fin
        | Some s -> Some (s, (n_fin - n_start))
      )
  )

  in down mouse_x mouse_y start_x start_y sq_width spacing 0 dim

(* Returns the square the mouse is in and board it is on, or None *)
let rec quantize_mouse () : (string * int * string) option =
  let (mouse_x, mouse_y) = mouse_pos () in
  let peg_grid_check = checkgrid mouse_x mouse_y 35 320 20 5 10 in
  let ship_grid_check = checkgrid mouse_x mouse_y 335 20 20 5 10 in
  match peg_grid_check, ship_grid_check with
  | (Some (s, i), _) -> Some (s, i, "pegboard")
  | (_, Some (s, i)) -> Some (s, i, "shipboard")
  | _ -> None

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

(* Draws a generic battleship grid *)
let draw_play_board x y =
  set_color color1;
  drawgrid x y 20 5 10 true;
  set_color color2;
  drawgrid x y 20 5 10 false;
  drawtextlist (x + 8) (y + 248) letters 12 25 true;
  drawtextlist (x - 13) (y + 230) numbers 12 25 false

(* Draws one peg in the indicated square *)
let draw_peg x y hit =
  let (m, s) = if hit then (color15, 0) else (color14, 0) in
  set_color s;
  fill_circle (x + 10) (y + 10) 8;
  set_color m;
  fill_circle (x + 10) (y + 10) 6

(* Draws the peg board, given a board *)
let draw_peg_grid board =
  draw_play_board 35 320;
  set_color 0;
  moveto 119 306;
  draw_string "Your guesses";
  let rec peg_traverse x y w s =

(* Draws the ship board, given a board *)
let draw_ship_grid board =
  draw_play_board 335 20;
  set_color 0;
  moveto 419 6;
  draw_string "Your board";
  let rec ship_traverse x y w s =

let draw_ship_piece x y hit identifier =
  let (l, m, d) = if hit then (color11, color12, color13)
  else (color8, color9, color10) in
  set_color m;
  fill_rect (x + 2) (y + 2) 16 16;
  drawline ((x + 3), (y + 3)) ((x + 3), (y + 17)) 2 l;
  drawline ((x + 3), (y + 17)) ((x + 17), (y + 17)) 2 l;
  drawline ((x + 3), (y + 2)) ((x + 18), (y + 2)) 2 d;
  drawline ((x + 18), (y + 2)) ((x + 18), (y + 17)) 2 d;
  moveto (x + 5) (y + 4);
  set_color d;
  draw_string identifier;
  moveto (x + 4) (y + 5);
  set_color color14;
  draw_string identifier

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
  (* Dark stats emboss *)
  drawline (325,325) (325, 525) 2 color6;
  drawline (325, 525) (575, 525) 2 color6;
  drawline (325, 535) (325, 575) 2 color6;
  drawline (325, 575) (575, 575) 2 color6;
  (* Light stats emboss *)
  drawline (325, 325) (575, 325) 2 color7;
  drawline (575, 325) (575, 525) 2 color7;
  drawline (325, 535) (575, 535) 2 color7;
  drawline (575, 535) (575, 575) 2 color7;
  (* Stats text *)
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
  (* Dark console bevels *)
  drawline (22, 22) (22, 277) 2 color6;
  drawline (22, 277) (278, 277) 2 color6;
  drawline (30, 28) (272, 28) 2 color6;
  drawline (272, 28) (272, 210) 2 color6;
  drawline (30, 218) (272, 218) 2 color6;
  drawline (272, 218) (272, 270) 2 color6;
  (* Light console bevels *)
  drawline (22, 22) (277, 22) 2 color7;
  drawline (277, 22) (278, 276) 2 color7;
  drawline (29, 29) (29, 210) 2 color7;
  drawline (29, 210) (272, 210) 2 color7;
  drawline (29, 218) (29, 270) 2 color7;
  drawline (29, 270) (272, 270) 2 color7;
  (* Console text *)
  set_color color5;
  moveto 128 240;
  draw_string "Console:";
  drawtextlist 70 180 ["Random command 1"; "Hello world"] 25 25 false

let draw_game game console_list =
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