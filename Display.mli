open Graphics
open Game

(*
  Draws a grid with the bottom left at x, y, square width w,
  square spacing s, dimensions d-by-d, and fill true or false
*)
val drawgrid : int -> int -> int -> int -> int -> bool -> unit

(*
  Draws a list of strings vertically or horizontally, with the
  bottom left of the first character at x, y, the list of strings
  sl, text size s, and text spacing sp, with across vs. down
  orientation true or false
*)
val drawtextlist : int -> int -> string list -> int -> int -> bool -> unit

(*
  Draws a peg at x, y (red or white, based on hit/miss bool)
*)
val draw_peg : int -> int -> bool -> unit

(*
  Draws a ship piece at x, y of type ship
*)
val draw_ship : int -> int -> ship -> unit

(*
  Draws the players' guesses on the top half of the board
*)
val draw_peg_board : board -> unit

(*
  Draws the players' ships on the bottom half of the board
*)
val draw_ship_board : board -> unit

(*
  Draws a blind for multiplayer, between players' turns
*)
val draw_blind: unit -> unit

(*
  Draws the game stats (Turn number, players' active ships), with
  columns for players and ship names
*)
val draw_stats: game -> unit

(*
  Draws the console with the last 4-5 lines of console text
*)
val draw_console : string list -> unit

(*
  Accepts game, which gives information about what board to draw,
  current player turn, all boards, players, ship positions, pegs.
*)
val draw_game : game -> unit