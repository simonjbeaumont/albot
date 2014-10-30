open Lwt
module Irc = Irc_client_lwt.Client

let ticket_re =
  let projs = ["CA"; "CP"; "SCTX"; "XOP"; "CAR"; "WP"] in
  let patt = Printf.sprintf "\\(%s\\)-[0-9]+" (String.concat "\\|" projs) in
  Re.compile (Re_emacs.re ~case:false patt)

let extract_ticket s =
  try
    let subs = Re.exec ticket_re s in
    Some (Re.get subs 0)
  with Not_found -> None

let jira_hostname = "issues.citrite.net"

let browser_uri_of_key key =
  let path = "/browse/" ^ key in
  Uri.make ~scheme:"https" ~host:jira_hostname ~path ()

let rest_uri_of_key key =
  let path = "/rest/api/latest/issue/" ^ key in
  Uri.make ~scheme:"https" ~host:jira_hostname ~path
    ~query:["fields",["summary"; "assignee"; "status"]] ()

type ticket = {
  key : string;
  url : string;
  summary : string;
  assignee : string;
  status : string;
}

let pretty_string_of_ticket t =
  Printf.sprintf
    "↳ %s [%s] \"%s\" is assigned to %s (%s)"
    t.key t.status t.summary t.assignee t.url

let ticket_of_json json =
  let open Yojson.Basic.Util in
  let j = Yojson.Basic.from_string json in
  let key = j |> member "key" |> to_string in
  let summary = j |> member "fields" |> member "summary" |> to_string in
  let status = j |> member "fields" |> member "status" |> member "name" |> to_string in
  let assignee =
    try j |> member "fields" |> member "assignee" |> member "displayName" |> to_string
    with Type_error _ -> "Unassigned"
  in
  let url = browser_uri_of_key key |> Uri.to_string in
  { key; url; summary; assignee; status; }

let reply_of_key key =
  let uri = rest_uri_of_key key in
  Cohttp_lwt_unix.Client.get uri >>= fun (_, body) ->
  Cohttp_lwt_body.to_string body >>= fun s ->
  ticket_of_json s |> pretty_string_of_ticket |> return

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
      reply_of_key ticket >>= fun reply ->
      let channels = message.Irc_message.params in
      Lwt_list.iter_p (fun c ->
        Irc.send_privmsg ~connection ~target:c ~message:reply
      ) channels
    | None -> return ()

