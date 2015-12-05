(*The ships used in the game, each ship is of type bool, bool
 * representing whether the ship has been 'hit'. True represents
 * that that part of the ship has been hit, while false represents
 * otherwise*)
type ship_name =
  |Carrier  |Battleship  |Cruiser
  |Destroyer  |Patrol

type ship = {name : ship_name ; mutable hit : bool}

(*Type square, which simulates a space on either a player's board
 * or the representation of the opponent's board. It is either a
 * Ship of type ship, an empty space, or a Peg of type bool*)
type square =
  |Ship of ship
  |Empty
  |Peg of bool

(*The position of the square; A5 for example is ('A',5)*)
type position = (char * int)

(*An association list of positions and squares; represents
 * the game's multiple boards. The board can either contain Ships
 * and empty squares, or Pegs and empty squares, depending on whether
 * it is a player's own board or a representation of an opponent's*)
type board = (position * square ref) list

type player_model =
  {board : board; pboard : board; mutable ships : ship list}

(* TODO: Document me *)
val create_model : unit -> player_model

(*creates empty board*)
val ship_cons : ship_name -> ship

(*Creates a new ship at the listed positions*)
val add_ship : ship_name -> position list -> player_model ->
    unit

(*Checks if all squares are empty on the board*)
val is_won : player_model -> bool

(*Checks if ship is sunk on the board*)
val is_sunk : ship_name -> player_model -> bool

(*Helper function that checks if the indicated position
 * is a ship within the board, returns true*)
val check_guess : position -> player_model -> bool

(*Updates opponent's board based on value given by check_guess. If
  the guessed position did not contain a ship, returns the board
  itself  *)
val update_board : position -> player_model -> unit

(*Updates peg board based on value given by check_guess. If the guess
 * did not hit a ship, then a Peg of false is placed on the board at
 * the given position. If it did, then a Peg of true is placed*)
val update_peg : position -> player_model -> bool -> unit
