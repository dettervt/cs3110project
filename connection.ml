open Unix

let open_connection sockaddr =
    let sock = (socket PF_INET SOCK_STREAM 0) in
    let _ = connect sock sockaddr in
    let i = in_channel_of_descr sock in
    let o = out_channel_of_descr sock in
    (i,o)

let shutdown_connection in_channel =
    shutdown (descr_of_in_channel in_channel) SHUTDOWN_SEND;;
