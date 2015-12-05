open Graphics
open Board
open Player
open Game

(*
  Takes current mouse coordinates and outputs the position they
  correspond to, or None if the mouse is not on a board square
*)
val quantize_mouse : unit -> (position * string) option

(*
  Accepts game, which gives information about what board to draw,
  current player turn, all boards, players, ship positions, pegs.
  Also accepts a string list representing the items in the console.
*)
val draw_game : game -> string list -> unit