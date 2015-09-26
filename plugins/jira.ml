open Lwt
module Irc = Irc_client_lwt

let find_all_matches re s =
  let rec aux acc pos =
    let open Re in
    match try exec ~pos re s |> get_all_ofs |> Array.to_list with _ -> [] with
    | [] -> acc
    | (i, j)::_ -> aux ((String.sub s i (j-i))::acc) j
  in
  aux [] 0 |> List.rev

let ticket_re =
  let projs = ["CA"; "CP"; "SCTX"; "XOP"; "CAR"; "WP"; "HFX"] in
  let patt = Printf.sprintf "\\(%s\\)-[0-9]+" (String.concat "\\|" projs) in
  Re.compile (Re_emacs.re ~case:false patt)

let extract_tickets = find_all_matches ticket_re

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
  try
    ticket_of_json s |> pretty_string_of_ticket |> return
  with _ -> Printf.sprintf "Couldn't find ticket: %s" key |> return

let name = "JIRA linker"

let rule = function
  | {Irc_message.command = Irc_message.PRIVMSG (_, msg)} ->
    Re.execp ticket_re msg
  | _ -> false

let run ~connection ~message =
  match message.Irc_message.command with
  | Irc_message.PRIVMSG (channel, msg) ->
    let tickets = extract_tickets msg in
    Lwt_list.map_p reply_of_key tickets >>=
    Lwt_list.iter_p (fun reply ->
      Irc.send_privmsg ~connection ~target:channel ~message:reply
    )
  | _ -> return ()
