open Board

type player = {num : string; model : Board.board;
 mutable guesses : position list}

let create_player s = 
    {name = s; model = Board.create_model () ;  guesses = []}

let is_guessed pos p = List.mem pos p.guesses

let add_guess pos p = p.guesses <- p.guesses @ [p]
