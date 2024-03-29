open Board
open Player

type game = {mutable current_player : int
; player1: Player.player
; player2: Player.player }

let inc_turn g = if g.current_player mod 2 = 0 then
  g.current_player <- 1 else g.current_player <- 2

let set_current g i =
    g.current_player <- i

let serialize_game g = 
  string_of_int g.current_player ^ "|||" ^
  Player.serialize_player g.player1 ^ "|||" ^
  Player.serialize_player g.player2 

let deserialize_game s = let lst = Str.split (Str.regexp "|||") s in match lst with
  |x::y::z::_ -> 
    {current_player = int_of_string x; player1 = deserialize_player y;
    player2 = deserialize_player z}
  |_ -> failwith "not possible"



