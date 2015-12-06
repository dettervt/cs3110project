open Display
open Game
open Connection
open Player
open Board
open Graphics

exception Invalid_Arguments;;
exception Not_Implemented of string;;
exception Local_Vs;;

(* Precondition: p1 and p2 are in the same either row or column *)
let get_dist (p1:position) (p2:position) : int =
    let (c1, i1) = p1 in
    let (c2, i2) = p2 in
    if(i1 = i2) then
    let num1 = int_of_char c1 in
    let num2 = int_of_char c2 in
    abs(num1 - num2)+1
    else
    abs(i1 - i2)+1

(* Get_between_X ensures p1 is less than p2 in whatever int/char order*)
(* Same int and c1 > c2 e.g. c1 = B c2 = A *)
let get_between_int p1 p2 : position list =
    let dist = get_dist p1 p2 in
    let (c, s) = p2 in
    let f (a:char) (i:int) : char =
        let j = int_of_char a in
        char_of_int (j+i) in
    (* I'm so sorry. For loops were being annoying. *)
    match dist with
    | 2 -> [p1; p2]
    | 3 -> [p1; ((f c 1), s); p2]
    | 4 -> [p1; ((f c 1), s); ((f c 2), s); p2]
    | 5 -> [p1; ((f c 1), s); ((f c 2), s); ((f c 3), s); p2]
    | _ -> failwith "This should never happen"

(* Same char and i1 > i2 *)
let get_between_char p1 p2 : position list =
    let dist = get_dist p1 p2 in
    let _ = Printf.printf "%d\n" dist in
    let (c,s) = p2 in
    match dist with
    | 2 -> [p1; p2]
    | 3 -> [p1; (c, s+1); p2]
    | 4 -> [p1; (c, s+1); (c, s+2); p2]
    | 5 -> [p1; (c, s+1); (c, s+2); (c, s+3); p2]
    | _ -> failwith "This should never happen"

(* Order positions and call helper functions *)
let get_between (p1:position) (p2:position) : position list =
    let (c1, i1) = p1 in
    let (c2, i2) = p2 in
    match (c1=c2, i2 > i1, c1>c2) with
    | (true, true, _) -> get_between_char p2 p1
    | (true, false, _) -> get_between_char p1 p2
    | (false, _, true) -> get_between_int p1 p2
    | (false, _, false) -> get_between_int p2 p1
    (*| _ -> failwith "This should never happen, either c1=c2 or i1=i2"*)


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
    match (c1=c2, i1=i2) with
    | (true, true) -> get_valid_p2 p1 pm shiplen
    | (true, false) -> if (get_dist (c1, i1) (c2, i2)) = shiplen then (c2,i2) else get_valid_p2 p1 pm shiplen
    | (false, true) -> if (get_dist (c1, i1) (c2, i2)) = shiplen then (c2,i2) else get_valid_p2 p1 pm shiplen
    | (false, false) -> get_valid_p2 p1 pm shiplen

let place_valid (shiplen:int) (pm:player_model) : position list =
    let pos1 = get_valid_pos "shipboard" pm in
    let pos2 = get_valid_p2 pos1 pm shiplen in
    get_between pos1 pos2



let place_ship (s:ship_name) (pm:player_model) : unit =
    match s with
    | Carrier -> add_ship s (place_valid 5 pm) pm
    | Battleship -> add_ship s (place_valid 4 pm) pm
    | Destroyer -> add_ship s (place_valid 3 pm) pm
    | Cruiser -> add_ship s (place_valid 3 pm) pm
    | Patrol -> add_ship s (place_valid 2 pm) pm

let place_ships (p:player) (game_model:game): unit =
    place_ship Carrier p.model;
    draw_game game_model [""];
    place_ship Battleship p.model;
    draw_game game_model [""];
    place_ship Destroyer p.model;
    draw_game game_model [""];
    place_ship Cruiser p.model;
    draw_game game_model [""];
    place_ship Patrol p.model;
    draw_game game_model [""]

let handle_local_vs _ : unit=
    open_battleship_window ();
    let p1 = Player.create_player (Sys.argv.(1)) in
    let p2 = Player.create_player (Sys.argv.(2)) in
    let game_model = {
    current_player =1;
    player1 = p1;
    player2 = p2} in
    draw_game game_model ["Player 1: place your ships"];
    place_ships (game_model.player1) game_model;
    set_current game_model 3;
    draw_game game_model [""];
    let _ = wait_next_event [Button_down] in
    set_current game_model 2;
    draw_game game_model ["Player 2: place your ships"];
    place_ships (game_model.player2) game_model;
    draw_game game_model [""]


(* Main method *)
try
    let arglen = Array.length Sys.argv in
    if(arglen = 1 || arglen > 5) then raise Invalid_Arguments;
    if(arglen = 2) then raise (Not_Implemented "Local AI");
    if(arglen = 3) then raise Local_Vs;
    if(arglen = 4) then raise (Not_Implemented "Net host");
    if(arglen = 5) then raise (Not_Implemented "Net conn");
with
    | Local_Vs -> (handle_local_vs ())
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
