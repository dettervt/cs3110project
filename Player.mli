open Board

(*Type player, contains the name of the player, the player's
 * board, the board that keeps track of the player's guesses
 * on the opponent's board, the list of guesses the player has
 * already made, whether or not the player is an AI,
 * and the difficulty of the AI (0 if not AI)*)

type player = {num : string; model : player_model;
 mutable guesses : position list}

(*Creates a player using a given name, whether or not it is an AI,
 * and the difficulty (0 if not AI)*)
val create_player : string  -> player

(*Returns true if given position is a part of the player's
 * already guessed list*)
val is_guessed : position -> player -> bool

(*Adds a position to the list of positions that the player has
 * already guessd*)
val add_guess : position -> player -> unit

val serialize_player : player -> string

val deserialize_player : string -> player
