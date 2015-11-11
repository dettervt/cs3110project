open Board
open Player

(*Represents the current state of the game. The current player is 
 * represented as an int, while both players playing the game are
 * also represented. Additionally, a turn counter is included,
 * incrementing with each move.*)
type game = {current_player : int 
; player1 : player
; player2 : player
; turn : int}

(*Increments the turn up by one*)
val inc_turn : int -> int



