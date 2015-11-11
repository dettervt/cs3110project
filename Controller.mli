open Graphics
open Display
open Game
open Player
open Board

(* gets mouse information - x, y, is clicked? *)
val collect_mouse : unit -> int * int * bool

(* takes number of players and AI level, returns initial game state *)
val create : int -> int -> game

(* function to run the game loop *)
val run_game : game -> game