require "Game"
require "Display"
require "Connection"

exception Invalid_Arguments;;
exception Not_Implemented of string;;


(* Main method *)
try
    let arglen = Array.length Sys.argv in
    if(arglen = 1 || arglen > 5) raise Invalid_Arguments;
    if(arglen = 2) then raise (Not_Implemented "Local AI");
    if(arglen = 3) then raise (Not_Implemented "Local VS");
    if(arglen = 4) then raise (Not_Implemented "Net host");
    if(arglen = 5) then raise (Not_Implemented "Net conn");
with
    | Not_Implemented s ->
        Printf.eprintf "%s not implemented.\n%!" s
    | Invalid_Argument ->
        let errmsg =
            "Usage:
            Local AI: local <playername>
            Local VS: local <p1name> <p2name>
            NET VS HOST: net host <playername>
            NET VS CONN: net conn <playername> <hostip>"
        in
        Printf.eprintf "%s\n%!" errmsg
    | _ -> exit 2
