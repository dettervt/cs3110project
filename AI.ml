open Board
open Player
open Game
open Random

let chars = ['A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J']

let populate_ai_board game : unit =
    ()

let randomize_guess game (guess:position ref) : unit =
    let () = Random.self_init () in
    guess := ((List.nth chars (Random.int 10)), (Random.int 10))

let random_guess game : position =
  let guesses = game.player2.guesses in
  let () = Random.self_init () in
  let guess = ref ('Z',0) in
  guess := ((List.nth chars (Random.int 10)), (Random.int 10));

  let _ = (while (List.mem (!guess) guesses) do
    randomize_guess game guess;
  done)

  in (!guess)
