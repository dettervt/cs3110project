open Display
open Game
open Connection
open Player
open Board
open Graphics
open AI

exception Invalid_Arguments;;
exception Not_Implemented of string;;
exception Local_Vs;;
exception Local_Ai;;
exception Net_Host;;
exception Net_Conn;;

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
    | Some (pos, s2) -> if (s=s2) then pos else get_valid_click s

(* Get an empty position on this player model's board *)
let rec get_valid_pos (s:string) (pm:player_model) : position =
    let pos = get_valid_click s in
    match check_guess pos pm with
    | true -> get_valid_pos s pm
    | false -> pos

let rec get_valid_guess (p:player) : position =
    let pos = get_valid_click "pegboard" in
    if is_guessed pos p then get_valid_guess p else pos

let rec get_valid_p2 (p1:position) (pm:player_model) (shiplen:int) : position =
    let (c1, i1) = p1 in
    let (c2, i2) = get_valid_pos "shipboard" pm in
    match (c1=c2, i1=i2) with
    | (true, true) -> get_valid_p2 p1 pm shiplen
    | (true, false) -> if (get_dist (c1, i1) (c2, i2)) = shiplen then (c2,i2) else get_valid_p2 p1 pm shiplen
    | (false, true) -> if (get_dist (c1, i1) (c2, i2)) = shiplen then (c2,i2) else get_valid_p2 p1 pm shiplen
    | (false, false) -> get_valid_p2 p1 pm shiplen

let place_valid (shiplen:int) (pm:player_model) (game_model:game) : position list =
    let pos1 = get_valid_pos "shipboard" pm in
    let _ = set_selected pos1 pm in
    let _ = draw_game game_model [""] in
    let pos2 = get_valid_p2 pos1 pm shiplen in
    get_between pos1 pos2

let place_ship (s:ship_name) (pm:player_model) (game_model:game) : unit =
    match s with
    | Carrier -> add_ship s (place_valid 5 pm game_model) pm
    | Battleship -> add_ship s (place_valid 4 pm game_model) pm
    | Destroyer -> add_ship s (place_valid 3 pm game_model) pm
    | Cruiser -> add_ship s (place_valid 3 pm game_model) pm
    | Patrol -> add_ship s (place_valid 2 pm game_model) pm

let place_ships (p:player) (game_model:game): unit =
    place_ship Carrier p.model game_model;
    draw_game game_model ["Place your battleship"];
    place_ship Battleship p.model game_model;
    draw_game game_model ["Place your destroyer"];
    place_ship Destroyer p.model game_model;
    draw_game game_model ["Place your cruiser"];
    place_ship Cruiser p.model game_model;
    draw_game game_model ["Place your patrol boat"];
    place_ship Patrol p.model game_model;
    draw_game game_model ["Finished placing"]

let do_guess pos curr opp : string =
    let hit = check_guess pos opp.model in
    let _ = update_peg pos curr.model hit in
    if hit then
    let _ = update_board pos opp.model in
    "hit"
    else "miss"

let rec aigameloop g last : bool =
    draw_game g [""];
    match g.current_player with
    | 1 ->
    begin
        let msg = ["It is your turn to guess!"] in
        let _ = draw_game g msg in
        let pos = get_valid_guess g.player1 in
        let _ = add_guess pos g.player1 in
        let h = do_guess pos g.player1 g.player2 in
        let h' = [h]@msg in
        let _ = draw_game g h' in
        let _ = wait_next_event [Button_down] in
        if is_won g.player2.model then
        let _ = set_current g 4 in
        let _ = aigameloop g 1 in
        false
        else
        let _ = set_current g 3 in
        let _ = aigameloop g 1 in
        true
    end
    | 2 ->
    begin
        let pos = random_guess g in
        let _ = add_guess pos g.player2 in
        let _ = do_guess pos g.player2 g.player1 in
        if is_won g.player1.model then
        let _ = set_current g 5 in
        let _ = aigameloop g 2 in
        false
        else
        let _ = set_current g 1 in
        let _ = aigameloop g 2 in
        false
    end
    | 4
    | 5 ->
    begin
        let _ = wait_next_event [Button_down] in
        false
    end
    | 8 -> failwith "AI placing! Never should happen"
    | 9 -> failwith "Waiting on AI"
    | _ -> failwith "Never should happen"

let rec localgameloop g last : bool=
    draw_game g [""];
    match g.current_player with
    | 1 ->
    begin
        let msg = ["It is your turn to guess!"] in
        let _ = draw_game g msg in
        let pos = get_valid_guess g.player1 in
        let _ = add_guess pos g.player1 in
        let h = do_guess pos g.player1 g.player2 in
        let h' = [h]@msg in
        let _ = draw_game g h' in
        let _ = wait_next_event [Button_down] in
        if is_won g.player2.model then
        let _ = set_current g 4 in
        let _ = localgameloop g 1 in
        false
        else
        let _ = set_current g 3 in
        let _ = localgameloop g 1 in
        true
    end
    | 2 ->
    begin
        let msg = ["It is your turn to guess!"] in
        let _ = draw_game g msg in
        let pos = get_valid_guess g.player2 in
        let _ = add_guess pos g.player2 in
        let h = do_guess pos g.player2 g.player1 in
        let h' = [h]@msg in
        let _ = draw_game g h' in
        let _ = wait_next_event [Button_down] in
        if is_won g.player1.model then
        let _ = set_current g 5 in
        let _ = localgameloop g 2 in
        false
        else
        let _ = set_current g 3 in
        let _ = localgameloop g 2 in
        true
    end
    | 3 ->
    begin
        let _ = wait_next_event [Button_down] in
        match last with
        | 1 -> set_current g 2; localgameloop g 1
        | 2 -> set_current g 1; localgameloop g 2
        | _ -> set_current g 2; localgameloop g 1
    end
    | 4
    | 5 ->
    begin
        let _ = wait_next_event [Button_down] in
        false
    end
    | _ -> failwith "Never should happen"

let handle_local_vs _ : unit=
    open_battleship_window ();
    let p1 = Player.create_player (Sys.argv.(1)) in
    let p2 = Player.create_player (Sys.argv.(2)) in
    let game_model = {
    current_player =1;
    player1 = p1;
    player2 = p2} in
    draw_game game_model ["Player 1: place your carrier"];
    place_ships (game_model.player1) game_model;
    set_current game_model 3;
    draw_game game_model [""];
    let _ = wait_next_event [Button_down] in
    set_current game_model 2;
    draw_game game_model ["Player 2: place your carrier"];
    place_ships (game_model.player2) game_model;
    draw_game game_model [""];
    set_current game_model 3;

    let playing = ref true in
    while (!playing) do
        (playing := localgameloop game_model 2)
    done

let rec network_host ic oc g : bool =
    draw_game g [""];
    match g.current_player with
    | 1 ->
    begin
        let msg = ["It is your turn to guess!"] in
        let _ = draw_game g msg in
        let pos = get_valid_guess g.player1 in
        let _ = add_guess pos g.player1 in
        let h = do_guess pos g.player1 g.player2 in
        let h' = [h]@msg in
        let _ = draw_game g h' in
        let _ = wait_next_event [Button_down] in
        (* CHECK WIN *)
        if is_won g.player2.model then
        let _ = set_current g 4 in
        (* Send *)
        let sg = serialize_game g in
        let _ = output_string oc (sg^"\n") in
        (* End *)
        let _ = shutdown_connection ic in
        let _ = network_host ic oc g in
        false
        (* ***************************** *)
        else
        (* Change *)
        let _ = set_current g 2 in
        (* Send *)
        let sg = serialize_game g in
        let _ = output_string oc (sg^"\n") in
        (* Change display *)
        let _ = set_current g 7 in
        let _ = draw_game g [""] in
        (* Get new*)
        let g' = deserialize_game (input_line ic) in
        let _ = network_host ic oc g' in
        true
    end
    | 4
    | 5 ->
    begin
    let _ = draw_game g [""] in
    let _ = wait_next_event [Button_down] in
        false
    end
    | _ -> failwith "This shouldn't actually happen"

let rec network_conn ic oc g : bool =
    draw_game g [""];
    match g.current_player with
    | 2 ->
    begin
        let msg = ["It is your turn to guess!"] in
        let _ = draw_game g msg in
        let pos = get_valid_guess g.player2 in
        let _ = add_guess pos g.player2 in
        let h = do_guess pos g.player2 g.player1 in
        let h' = [h]@msg in
        let _ = draw_game g h' in
        let _ = wait_next_event [Button_down] in
        (* CHECK WIN *)
        if is_won g.player1.model then
        (* Change *)
        let _ = set_current g 5 in
        (* Send *)
        let sg = serialize_game g in
        let _ = output_string oc (sg^"\n") in
        (* End *)
        let _ = shutdown_connection ic in
        let _ = network_conn ic oc g in
        false
        (* *************************** *)
        else
        (* Change *)
        let _ = set_current g 1 in
        (* Send *)
        let sg = serialize_game g in
        let _ = output_string oc (sg^"\n") in
        (* Change display *)
        let _ = set_current g 7 in
        let _ = draw_game g [""] in
        (* Get new *)
        let g' = deserialize_game (input_line ic) in
        let _ = network_conn ic oc g' in
        true
    end
    | 4
    | 5
    | _ -> failwith "This shouldn't actually happen either"
    (* Shutdown connection ic on end*)

let handle_network_host _ : unit =
    let server_hostname = Unix.gethostname () in
    let serverport = Sys.argv.(3) in
    let port = int_of_string serverport in
    let my_addr = (Unix.gethostbyname(server_hostname)).Unix.h_addr_list.(0) in
    let my_addr_s = Unix.string_of_inet_addr my_addr in
    let sockaddr = Unix.ADDR_INET(my_addr, port) in
    let serversock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in

    open_battleship_window ();
    let p1 = Player.create_player ("") in
    let p2 = Player.create_player ("") in
    let game_model ={
    current_player=1;
    player1 = p1;
    player2 = p2;} in
    let _ = draw_game game_model [my_addr_s] in
    set_current game_model 6;
    draw_game game_model [""];
    (* Displaying waiting for connection *)
    let _ = Unix.bind serversock sockaddr in
    let _ = Unix.listen serversock 1 in

    (* Wait for connection *)
    let (s, _) = Unix.accept serversock in
    let inchan = Unix.in_channel_of_descr s and
    outchan = Unix.out_channel_of_descr s in

    (* Send Game *)
    let sg = serialize_game game_model in
    let _ = output_string outchan (sg^"\n") in
    let _ = flush outchan in

    (* Wait for game *)
    let g' = deserialize_game (input_line inchan) in

    let playing = ref true in
    while (!playing) do
        (playing := network_host inchan outchan g')
    done

let handle_network_conn _ : unit =
    let serverip = Sys.argv.(3) in
    let serverport = Sys.argv.(4) in
    let server_addr = Unix.inet_addr_of_string serverip in
    let port = int_of_string serverport in
    let sockaddr = Unix.ADDR_INET(server_addr, port) in
    let ic, oc = open_connection sockaddr in
    let game_model = deserialize_game(input_line ic) in
    (* Game gotten *)
    let _ = open_battleship_window () in
    let _ = draw_game game_model ["Player 2: place your carrier"] in
    let _ = place_ships game_model.player2 game_model in
    let _ = draw_game game_model [""] in
    (* Game recreated *)
    (* Send game *)
    let _ = set_current game_model 1 in (* Player 1's turn *)
    let sg = serialize_game game_model in
    let _ = output_string oc (sg^"\n") in
    let _ = flush oc in

    (* Displays waiting for other player *)
    let _ = set_current game_model 7 in
    let _ = draw_game game_model [""] in

    let playing = ref true in
    while(!playing) do
        (playing := network_conn ic oc game_model)
    done

let handle_ai _ : unit =
    open_battleship_window ();
    let p1 = Player.create_player (Sys.argv.(1)) in
    let p2 = Player.create_player "AI" in
    let game_model = {
    current_player=1;
    player1 = p1;
    player2 = p2} in
    draw_game game_model ["Player 1: plae your carrier"];
    place_ships (game_model.player1) game_model;
    set_current game_model 8; (* AI placing *)
    draw_game game_model [""];
    (* AI placement here *)
    set_current game_model 3;
    let playing = ref true in
    while (!playing) do
        (playing := aigameloop game_model 2)
    done

(* Main method *)
try
    let arglen = Array.length Sys.argv in
    if(arglen = 1 || arglen > 5) then raise Invalid_Arguments;
    if(arglen = 2) then raise (Not_Implemented "Local AI");
    if(arglen = 3) then raise Local_Vs;
    if(arglen = 4) then raise (Not_Implemented "Net host");
    if(arglen = 5) then raise (Not_Implemented "Net conn");
with
    | Local_Ai -> (handle_ai ())
    | Local_Vs -> (handle_local_vs ())
    | Not_Implemented s ->
        Printf.eprintf "%s not implemented.\n%!" s
    | Invalid_Arguments ->
        let errmsg =
            "Usage:
            Local AI: local <playername>
            Local VS: local <p1name> <p2name>
            NET VS HOST: net host <port>
            NET VS CONN: net conn <hostip> <hostport>"
        in
        Printf.eprintf "%s\n%!" errmsg
