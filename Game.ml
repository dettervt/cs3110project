open Board
open Player

type game = {mutable current_player : int
; player1: Player.player
; player2: Player.player }

let inc_turn g = if g.current_player mod 2 = 0 then
  g.current_player <- 1 else g.current_player <- 2

let set_current g i =
    g.current_player <- i
