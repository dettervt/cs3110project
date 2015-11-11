open Board.mli
open Player.mli

(*Types*)
type game = {current_player : int 
; player1 : player
; player2 : player
; turn : int}

(*Increments the turn up by one*)
val inc_turn : int -> int



