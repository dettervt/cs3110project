(*open Display*)
open Game
open Connection

exception Invalid_Arguments;;
exception Not_Implemented of string;;

let handle_local_vs () =
    let p1 = Player.create_player (Sys.argv.(1)) in
    let p2 = Player.create_player (Sys.argv.(2)) in
    let game_model = {
    current_player =1;
    player1 = p1;
    player2 = p2} in
    inc_turn game_model


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
