open Board

type player = {num : string; model : Board.board;
 guesses : position list}

let create_player s = {name = s; 
 model = Board.create_board () ;
 mutable guesses = []}

let is_guessed pos p = List.mem pos p.guesses

