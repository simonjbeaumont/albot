open Lwt
module Irc = Irc_client_lwt.Client

let jira_base_url = "https://issues.citrite.net"
let ticket_re =
  let projs = ["CA"; "CP"; "SCTX"; "XOP"; "CAR"; "WP"] in
  let patt = Printf.sprintf "\\(%s\\)-[0-9]+" (String.concat "\\|" projs) in
  Re.compile (Re_emacs.re ~case:false patt)

let extract_ticket s =
  try
    let subs = Re.exec ticket_re s in
    Some (Re.get subs 0)
  with Not_found -> None

let name = "JIRA linker"

let rule m = m.Irc_message.command = "PRIVMSG" &&
  match m.Irc_message.trail with
  | Some msg -> Re.execp ticket_re msg
  | None -> false

let run ~connection ~message =
  match message.Irc_message.trail with
  | None -> return ()
  | Some msg ->
    match extract_ticket msg with
    | Some ticket ->
      let reply = String.concat "/" [jira_base_url; "browse"; ticket] in
      let channels = message.Irc_message.params in
      Lwt_list.iter_p (fun c ->
        Irc.send_privmsg ~connection ~target:c ~message:reply
      ) channels
    | None -> return ()

