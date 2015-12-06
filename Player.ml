open Board

type player = {num : string; model : Board.player_model;
 mutable guesses : position list}

let create_player s =
    {num = s; model = create_model () ;  guesses = []}

let is_guessed pos p = List.mem pos p.guesses

let add_guess pos p = p.guesses <- p.guesses @ [pos]

let serialize_player p = p.num ^ "||" ^
  Board.serialize_board p.model ^ "||" ^
  List.fold_left 
   (fun acc a -> (Board.serialize_position a )^"|+|"^) "" p.guesses
