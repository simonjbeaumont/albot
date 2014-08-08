let logfile = Filename.concat (Sys.getcwd ()) "chat.log"

let time () =
  let open Unix in
  let now = gmtime (gettimeofday ()) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" (now.tm_year+1900)
    (now.tm_mon+1) now.tm_mday now.tm_hour now.tm_min now.tm_sec

let string_of_message {Irc_message.prefix; command; params; trail} =
  let string_of_string_opt = function Some s -> s | None -> "None" in
  let string_of_string_list l = Printf.sprintf "[%s]" (String.concat "; " l) in
  Printf.sprintf "%s %s %s %s" (string_of_string_opt prefix) command
    (string_of_string_list params) (string_of_string_opt trail)

let name = "Logger"
let rule m = m.Irc_message.command = "PRIVMSG"
let run ~connection ~message =
  let c = open_out_gen [Open_append; Open_creat; Open_text] 0o644 logfile in
  try
    Printf.fprintf c "%s\t%s\n%!" (time ()) (string_of_message message);
    close_out c
  with _ ->
    close_out_noerr c

