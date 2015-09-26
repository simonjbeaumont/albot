open Lwt

let logfile = Filename.concat (Sys.getcwd ()) "chat.log"

let time () =
  let open Unix in
  let now = gmtime (gettimeofday ()) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" (now.tm_year+1900)
    (now.tm_mon+1) now.tm_mday now.tm_hour now.tm_min now.tm_sec

let name = "Logger"
let rule = function
  | {Irc_message.command = Irc_message.PRIVMSG _} -> true
  | _ -> false

let run ~connection ~message =
  let open Lwt_io in
  Printf.printf "1";
  with_file ~flags:Unix.([O_APPEND; O_CREAT]) ~mode:Output logfile (fun c ->
  Printf.printf "2";
    fprintf c "%s\t%s\n%!" (time ()) (Irc_message.to_string message) >>= flush_all
  )
