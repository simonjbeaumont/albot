let logfile = Filename.concat (Sys.getcwd ()) "chat.log"

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
    Printf.fprintf c "%s\n%!" (string_of_message message);
    close_out c
  with _ ->
    close_out_noerr c


