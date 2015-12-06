open Board
open Player

(*Represents the current state of the game. The current player is
 * represented as a mutable int, while both players playing the game are
 * also represented. *)
type game = {mutable current_player : int
; player1 : player
; player2 : player}

(*Increments the turn up by one*)
val inc_turn : game -> unit

(* Sets the current turn to given int*)
val set_current : game -> int -> unit

(*Serializes the game*)
val serialize_game : game -> string

val deserialize_game : string -> game


