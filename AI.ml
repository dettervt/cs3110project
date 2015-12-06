open Board
open Player
open Game
open Random

let chars = ['A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J']

let populate_ai_board game : unit =

let random_guess game : position =
  let guesses = Game.player2.guesses in
  let () = Random.self_init () in
  let guess = ((List.nth chars (Random.int 10)), (Random.int 10)) in
  let () = (while (List.mem guess guesses) do
    let guess = ((List.nth chars (Random.int 10)), (Random.int 10))
  done) in guess