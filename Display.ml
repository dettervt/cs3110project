open Graphics
open Board
open Player
open Game

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
                (dim: int) : position option =
  let rec across mx my sx sy w s n_start n_fin : char option =
  (
    match (n_start < n_fin) with
    | false -> None
    | true ->
      if (mx >= (sx + ((n_start * (w + s))))) &&
        (mx <= (sx + (((n_start + 1) * w) + (n_start * s)))) &&
        (my >= sy) &&
        (my <= (sy + w)) then
        Some (String.get (List.nth letters n_start) 0)
      else
        across mx my sx sy w s (n_start + 1) n_fin
  )

  and down mx my sx sy w s n_start n_fin : (char * int) option =
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
let rec quantize_mouse () : (position * string) option =
  let _ = wait_next_event [Button_down] in
  let (mouse_x, mouse_y) = mouse_pos () in
  let peg_grid_check = checkgrid mouse_x mouse_y 35 320 20 5 10 in
  let ship_grid_check = checkgrid mouse_x mouse_y 335 20 20 5 10 in
  match peg_grid_check, ship_grid_check with
  | (Some (c, i), _) -> Some ((c, i), "pegboard")
  | (_, Some (c, i)) -> Some ((c, i), "shipboard")
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

(*
  Opens the window for a game of battleship,
  turns off default 'flicker' screen updating
*)
let open_battleship_window () =
  let () = open_graph " 600x600" in
  set_window_title "Battleship";
  auto_synchronize false

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
  let rec peg_traverse x y w s d count board =
  let (xc, yc) = (((count/10) * (w + s)), (225 - ((count mod d) * (w + s)))) in
  let (xc, yc) = (xc + x, yc + y) in
    match board with
    | [] -> ()
    | h::t ->
      let () =
      (
        match !(snd h) with
        | Peg b -> draw_peg xc yc b
        | _ -> ()
      )
      in peg_traverse x y w s d (count + 1) t

  in peg_traverse 35 320 20 5 10 0 board

(* Draws one ship segment *)
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

(*
  Draws a green square to indicated the currently
  selected square, for ship placement
*)
let draw_selection x y =
  set_color color5;
  fill_rect (x + 1) (y + 1) 18 18;
  set_color 0

(* Draws the ship board, given a board *)
let draw_ship_grid board =
  draw_play_board 335 20;
  set_color 0;
  moveto 419 6;
  draw_string "Your board";
  let rec ship_traverse x y w s d count board =
  let (xc, yc) = (((count/10) * (w + s)), (225 - ((count mod d) * (w + s)))) in
  let (xc, yc) = (xc + x, yc + y) in
    match board with
    | [] -> ()
    | h::t ->
      let () =
      (
        match !(snd h) with
        | Ship c ->
        (
          match c.name with
          | Carrier ->
            draw_ship_piece xc yc c.hit "CA"
          | Battleship ->
            draw_ship_piece xc yc c.hit "BS"
          | Cruiser ->
            draw_ship_piece xc yc c.hit "CR"
          | Destroyer ->
            draw_ship_piece xc yc c.hit "DE"
          | Patrol ->
            draw_ship_piece xc yc c.hit "PB"
        )
        | Selected -> draw_selection xc yc
        | _ -> ()
      )
      in ship_traverse x y w s d (count + 1) t

  in ship_traverse 335 20 20 5 10 0 board

(* Draws the blind in-between players' turns on local multiplayer *)
let draw_blind () =
  set_color color4;
  fill_rect 0 0 600 600;
  set_color 0;
  moveto 256 300;
  draw_string "Click when ready"

(* Given a winning player, draws the win screen *)
let draw_win_screen winning_player =
  set_color color4;
  fill_rect 0 0 600 600;
  set_color 0;
  moveto 250 300;
  let text = "Player " ^ (string_of_int winning_player) ^ " wins!" in
  draw_string text

(* Draws grey screens for types of network waiting *)
let draw_net_wait in_game =
  set_color color4;
  fill_rect 0 0 600 600;
  set_color 0;
  if in_game then
    let () =
    moveto 211 300;
    draw_string "Waiting for opponent's guess..."
    in ()
  else
    let () =
    moveto 235 300;
    draw_string "Waiting for opponent..."
    in ()

(* Draws grey screens for types of AI waiting *)
let draw_AI_wait guessing =
  set_color color4;
  fill_rect 0 0 600 600;
  set_color 0;
  if guessing then
    let () =
    moveto 229 300;
    draw_string "Waiting for AI's guess..."
    in ()
  else
    let () =
    moveto 208 300;
    draw_string "Waiting for AI to place ships..."
    in ()

(*
  Helper function for draw_stats to draw red text
  over a player's destroyed ships
*)
let rec draw_ship_statuses n game player_num x y y_spacing =
  if n = 5 then () else
  let model =
    if player_num = 1 then
      (game.player1.model)
    else
      (game.player2.model)
  in
    let ship_to_check =
    (if n = 0 then Carrier else if n = 1 then Battleship
    else if n = 2 then Cruiser else if n = 3 then Destroyer
    else Patrol) in
    match (is_sunk ship_to_check model) with
    | true ->
        moveto (x - 1) ((y + 1) - (y_spacing * n));
        set_color color15;
        draw_string (List.nth shipnames n);
        draw_ship_statuses (n + 1) game player_num x y y_spacing
    | false ->
        draw_ship_statuses (n + 1) game player_num x y y_spacing

(*
  Draws the status box in the upper-right quadrant
  of the graphics window
*)
let draw_stats game =
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
  let turn_indic_string =
    "It is currently Player " ^
    string_of_int(game.current_player) ^
    "'s turn!" in
  draw_string turn_indic_string;
  set_color color3;
  drawtextlist 355 465 shipnames 25 25 false;
  draw_ship_statuses 0 game 1 355 465 25;
  set_color color3;
  drawtextlist 485 465 shipnames 25 25 false;
  draw_ship_statuses 0 game 2 485 465 25

(* Draws the console in the lower left, with the last 6 console messages *)
let draw_console command_list =
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
  let rec first_n_rev l n =
    match n with
    | 0 -> []
    | x -> (List.nth l (x - 1))::(first_n_rev l (n - 1))
  in
  let commands_for_display =
    let lenl = ((min 6 (List.length command_list))) in
    first_n_rev command_list lenl
  in drawtextlist 70 180 commands_for_display 25 25 false

(*
  Draws the entire game on the screen, given the current
  game state and list of console commands
*)
let draw_game game console_list =
  set_color color14;
  fill_rect 0 0 600 600;
  let () =
  (
    (* Player 1 viewpoint *)
    if (game.current_player) = 1 then
      let () =
      draw_peg_grid (game.player1.model.pboard);
      draw_ship_grid (game.player1.model.board);
      drawline (0, 300) (600, 300) 3 0;
      drawline (300, 0) (300, 600) 3 0;
      draw_stats game;
      draw_console console_list
      in ()
    (* Player 2 viewpoint *)
    else if (game.current_player) = 2 then
      let () =
      draw_peg_grid (game.player2.model.pboard);
      draw_ship_grid (game.player2.model.board);
      drawline (0, 300) (600, 300) 3 0;
      drawline (300, 0) (300, 600) 3 0;
      draw_stats game;
      draw_console console_list
      in ()
    (* Blind between local turns *)
    else if (game.current_player) = 3 then
      draw_blind ()
    (* Player 1 win screen *)
    else if (game.current_player) = 4 then
      draw_win_screen 1
    (* Player 2 win screen *)
    else if (game.current_player) = 5 then
      draw_win_screen 2
    (* Waiting for opponent to connect screen *)
    else if (game.current_player) = 6 then
      draw_net_wait false
    (* Waiting for opponent to make a guess screen *)
    else if (game.current_player) = 7 then
      draw_net_wait true
    (* Waiting for AI to place ships *)
    else if (game.current_player) = 8 then
      draw_AI_wait false
    (* Waiting for AI to make a move *)
    else if (game.current_player) = 9 then
      draw_AI_wait true
    (* This shouldn't happen *)
    else
      failwith "Invalid game state"
  )
    (* Updates screen *)
    in synchronize ()