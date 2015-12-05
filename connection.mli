open Unix

(*
    Some connection code from
http://caml.inria.fr/pub/docs/oreilly-book/html/book-ora187.html
 *)

(* Close the socket connection associated with the given in_channel *)
val shutdown_connection :  (in_channel) -> unit

(* Opens a new connection given a socket address
   Return the in and out channels
*)
val open_connection : (Unix.sockaddr) -> (in_channel * out_channel)
