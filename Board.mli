(*The ships used in the game, each ship is of type bool, bool
 * representing whether the ship has been 'hit'. True represents
 * that that part of the ship has been hit, while false represents
 * otherwise*)
type ship =
  |Carrier of bool |Battleship of bool |Cruiser of bool
  |Destroyer of bool |Patrol of bool

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
type board = (position * square) list

(*creates empty board*)
val create_empty : board

(*Creates a new ship at the listed positions*)
val create_ship : ship -> position list -> board -> board

(*Checks if all squares are empty on the board*)
val is_won : board -> bool

(*Checks the number of sunk ships*)
val num_ships_dead : board -> int

(*Checks if ship is sunk on the board*)
val is_sunk : ship -> board -> bool

(*Helper function that checks if the indicated position
 * is a ship within the board, returns true*)
val check_guess : position -> board -> bool

(*Updates opponent's board based on value given by check_guess. If
  the guessed position did not contain a ship, returns the board
  itself  *)
val update_board : position -> board -> bool -> board

(*Updates peg board based on value given by check_guess. If the guess
 * did not hit a ship, then a Peg of false is placed on the board at
 * the given position. If it did, then a Peg of true is placed*)
val update_peg : position -> board -> bool -> board
