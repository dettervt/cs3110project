open Display
open Game
open Connection
open Player
open Board

exception Invalid_Arguments;;
exception Not_Implemented of string;;

(* Precondition: p1 and p2 are in the same either row or column *)
let get_dist (p1:position) (p2:position) : int =
    let (c1, i1) = p1 in
    let (c2, i2) = p2 in
    if(c1 = c2) then
    let num1 = int_of_char c1 in
    let num2 = int_of_char c2 in
    abs(num1 - num2)+1
    else
    abs(i1 - i2)+1


(* Get a position on the desired board *)
let rec get_valid_click (s:string) : position =
    match quantize_mouse () with
    | None -> get_valid_click s
    | Some (pos, s) -> if (s=s) then pos else get_valid_click s

(* Get an empty position on this player model's board *)
let rec get_valid_pos (s:string) (pm:player_model) : position =
    let pos = get_valid_click s in
    match check_guess pos pm with
    | true -> get_valid_pos s pm
    | false -> pos

let rec get_valid_p2 (p1:position) (pm:player_model) (shiplen:int) : position =
    let (c1, i1) = p1 in
    let (c2, i2) = get_valid_pos "shipboard" pm in
    if (c1=c2 && i1<>i2) || (i1=i2 && c1<>c2) then
    if get_dist (c1,i1) (c2,i2) = shiplen then (c2,i2)
    else get_valid_p2 p1 pm shiplen
    else get_valid_p2 p1 pm shiplen

let place_valid (shiplen:int) (pm:player_model) : position list =
    let pos1 = get_valid_pos "shipboard" pm in
    let pos2 = get_valid_p2 pos1 pm shiplen in
    [pos1; pos2] (*TODO*)


let place_ship (s:ship_name) (pm:player_model) : unit =
    match s with
    | Carrier -> ()
    | Battleship -> ()
    | Destroyer -> ()
    | Cruiser -> ()
    | Patrol -> ()

let place_ships (p:player) : unit =
    place_ship Carrier p.model;
    place_ship Battleship p.model;
    place_ship Destroyer p.model;
    place_ship Cruiser p.model;
    place_ship Patrol p.model

let handle_local_vs () =
    let p1 = Player.create_player (Sys.argv.(1)) in
    let p2 = Player.create_player (Sys.argv.(2)) in
    let game_model = {
    current_player =1;
    player1 = p1;
    player2 = p2} in
    place_ships(game_model.player1);
    place_ships(game_model.player2)


(* Main method *)
try
    let arglen = Array.length Sys.argv in
    if(arglen = 1 || arglen > 5) then raise Invalid_Arguments;
    if(arglen = 2) then raise (Not_Implemented "Local AI");
    if(arglen = 3) then raise (Not_Implemented "Local VS");
    if(arglen = 4) then raise (Not_Implemented "Net host");
    if(arglen = 5) then raise (Not_Implemented "Net conn");
with
    | Not_Implemented s ->
        Printf.eprintf "%s not implemented.\n%!" s
    | Invalid_Arguments ->
        let errmsg =
            "Usage:
            Local AI: local <playername>
            Local VS: local <p1name> <p2name>
            NET VS HOST: net host <playername>
            NET VS CONN: net conn <playername> <hostip>"
        in
        Printf.eprintf "%s\n%!" errmsg
