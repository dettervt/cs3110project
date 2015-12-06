
type ship_name =
  |Carrier |Battleship |Cruiser
  |Destroyer  |Patrol

type ship = {name : ship_name; mutable hit : bool}

type square =
  |Ship of ship
  |Selected
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

let create_model () = let pos = position_list in
  let shp_bd = List.fold_left (fun acc a -> acc@[(a, ref Empty)]) [] pos in
  let peg_bd = List.fold_left (fun acc a -> acc@[(a, ref Empty) ]) [] pos in
  {board = shp_bd; pboard = peg_bd ; ships = []}

let set_selected pos pm : unit =
  (List.assoc pos pm.board) := (Selected)

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

let serialize_position p = let (c,i) = p in (String.make 1 c)^(string_of_int i)^"."

let matchtf i = match i with
  |true -> "T"
  |false -> "F"

let serialize_ship s = match s.name with
  |Carrier -> "A" ^ (matchtf s.hit) ^ "$%"
  |Battleship -> "B" ^ (matchtf s.hit) ^ "$%"
  |Destroyer -> "D" ^ (matchtf s.hit) ^ "$%"
  |Cruiser -> "C" ^ (matchtf s.hit) ^ "$%"
  |Patrol -> "Z"^ (matchtf s.hit) ^ "$%"


let serialize_square sq = match !sq with
  |Ship a -> serialize_ship a
  |Empty -> "E"
  |Peg p -> "P"^(matchtf p)
  |Selected -> "S"

let serialize_entry a = (serialize_position fst a)^"#"^
  (serialize_square snd)^"##"

let serialize_board b = List.fold_left
  (fun acc a -> acc^(serialize_entry a)) "" b

let serialize_model m = serialize_board m.board ^ "###" ^
  serialize_board m.pboard ^ "###" ^
  List.fold_left (fun acc a -> acc^(serialize_ship a)) "" m.ships

let deserialize_position s = let first = String.get str 0 in let sec =
  String.get str 1 in (first, int_of_char sec) 

let deserialize_ship str = let first = String.get str 0 in let sec = 
  String.get str 1 in match first with
    |'A' ->  {name = Carrier;hit = (sec = 'T')}
    |'B' ->  {name = Battleship;hit = (sec = 'T')}
    |'C' ->  {name = Destroyer;hit = (sec = 'T')}
    |'D' ->  {name = Cruiser;hit = (sec = 'T')}
    |'Z' ->  {name = Patrol;hit = (sec = 'T')}
    |_ -> failwith "not possible"

let deserialize_square str = let lst =

let deserialize_entry str = let lst = 
  Str.split (Str.regex "#") str in 
    (deserialize_position (List.hd lst), deserialize_square (List.tl lst))

let deserialize_board str = let lst = 
  Str.split (Str.regex "##") str in
    List.fold_left (fun acc a -> acc@(deserialize_entry a)) [] lst

let deserialize_model str = let lst = 
  Str.split (Str.regex "###") str in match lst with
    |x::y::z::_ -> {board = deserialize_board x;
      pboard = deserialize_board y; ships = 
      List.fold_left (fun acc a -> acc@(deserialize_ship a)) [] 
        (Str.split (Str.regexp "$%") z)}












