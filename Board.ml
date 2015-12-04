
type ship_name = 
  |Carrier |Battleship |Cruiser 
  |Destroyer  |Patrol 

type ship = {name : ship_name; mutable hit : bool}

type square = 
  |Ship of ship
  |Empty 
  |Peg of bool 

type position = (char * int)

type board = (position * square ref) list

type player_model = {board : board; 
  pboard : board; mutable ships : ship list}

let position_list = 
  let yaxis = ['A';'B';'C';'D';'E';'F';'G';'H';'I';'J'] in
  let xaxis = [10;9;8;7;6;5;4;3;2;1] in
    let rec matcher xs ys = match ys with
      |[] -> []
      |h::t -> List.fold_left (fun acc x -> [(h,x)]@acc) [] xs
        @ matcher xs t
  in matcher xaxis yaxis


let ship_cons = fun s -> {name = s; hit = false}

let create_board () = let pos = position_list in 
  let shp_bd = List.fold_left (fun acc a -> acc@[(a, ref Empty)]) [] pos in
  let peg_bd = List.fold_left (fun acc a -> acc@[(a, ref Empty) ]) [] pos in
  {board = shp_bd; pboard = peg_bd ; ships = []}

let rec add_ship shp plist bd = match plist with
  |[] -> ()
  |h::t -> let s = ship_cons shp in
    bd.ships <- bd.ships @ [s]; 
    (List.assoc h bd.board) := (Ship(s)) ; add_ship shp t bd

let is_won bd = let ships = bd.ships in let rec matcher shps = 
  match shps with
    |[] -> true
    |h::t -> if h.hit = false then false else matcher t
  in matcher ships

let is_sunk shp_name bd = let shps = bd.ships in 
  List.fold_left (fun acc a -> if a.name = shp_name then
    a.hit&&acc else true&&acc) true shps

let check_guess pos bd = match !(List.assoc pos bd.board) with
  |Ship a -> true
  |_ -> false

let update_board pos bd = let a = !(List.assoc pos bd.board) in 
  match a with
    |Ship x -> x.hit <- true
    |_ -> ()

let update_peg pos bd guessed = List.assoc pos bd.pboard := (Peg(guessed))








