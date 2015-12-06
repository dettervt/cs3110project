open Board
open Player
open Game
open Graphics


let rec gameloop game  = match game.current_player with
  |1 -> let peg = check_guess (*Position here*) (game.player2.model) in
    update_peg (*position*) (game.player1.model) (peg) ;
    update_board (*pos*) (game.player2.board) ;
    if is_won (game.player2.model) then (set_current game 4) else
        set_current game 2 ; gameloop game
  |2 -> let peg = check_guess (*position here*) (game.player1.model) in
    update_peg (*position here*) (game.player2.model) (peg) ;
    update_board (*position*) (game.player1.model) ;
    if is_won (game.player1.model) then (set_current game 5) else
        set_current game 1 ; gameloop game
  |4 -> (**)
  |5 -> (**)
  |3 -> (**)
  |_ -> failwith "bad game state"




