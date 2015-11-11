(*Types*)
type ship =
  |Destroyer of bool |Carrier of bool |Submarine of bool
  |Cruiser of bool |Battleship of bool

type square =
  |Ship of ship 
  |Empty
  |Peg of bool 

type position = (char * int)

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

(*Updates opponent's board based on value given by check_guess *)
val update_board : position -> board -> bool -> board

(*Updates peg board based on value given by check_guess*)
val update_peg : position -> board -> bool -> board
