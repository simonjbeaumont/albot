open Lwt
module Irc = Irc_client_lwt.Client

type key = {
  user : string;
  repo : string;
  number : string;
}

let string_of_key k = Printf.sprintf "%s/%s #%s" k.user k.repo k.number

let pullreq_re =
  let open Re in
  let slash = char '/' in
  let name = rep1 (compl [slash; space]) in
  let num = rep1 digit in
  let pull = str "pull" in
  let r = seq [slash; group name; slash; group name; slash; pull; slash; group num] in
  compile (no_case r)

let find_all_matches re s =
  let open Re in
  let rec aux acc pos =
    match try exec ~pos re s |> get_all_ofs |> Array.to_list with _ -> [] with
    | [] -> acc
    | (i, j)::_ -> aux ((String.sub s i (j-i))::acc) j
  in
  aux [] 0 |> List.rev

let extract_pullreqs s =
  find_all_matches pullreq_re s |> List.fold_left (fun acc key ->
    match Re.exec pullreq_re key |> Re.get_all |> Array.to_list with
    | _::user::repo::number::[] -> {user; repo; number}::acc
    | _ -> acc
  ) []

let pretty_string_of_pr (p : Github_t.pull) : string =
  let string_of_opt f = function None -> "None" | Some x -> f x in
  let string_of_merged_closed = function
    | None, None -> "Open"
    | Some _, _ -> "Merged"
    | _, Some _ -> "Closed"
  in
  let open Github_t in
  Printf.sprintf
    "↳ %s/%s #%d for branch [%s]: \"%s\" [%s] (%s)"
    (p.pull_base.branch_user |> string_of_opt (fun u -> u.user_login))
    (p.pull_base.branch_repo |> string_of_opt (fun r -> r.repository_name))
    p.pull_number
    p.pull_base.branch_ref
    p.pull_title
    (string_of_merged_closed (p.pull_merged_at, p.pull_closed_at))
    p.pull_diff_url

let pr_of_key {user; repo; number} : Github_t.pull =
  Github.Pull.get user repo (int_of_string number) ()
  |> Github.Monad.run |> Lwt_main.run |> Github.Response.value

let reply_of_key key =
  try
    pr_of_key key |> pretty_string_of_pr
  with _ ->
    Printf.sprintf "Couldn't find pull request: %s" (string_of_key key)

let name = "Pull request linker"

let rule m = m.Irc_message.command = "PRIVMSG" &&
  match m.Irc_message.trail with
  | Some msg -> Re.execp pullreq_re msg
  | None -> false

let run ~connection ~message =
  match message.Irc_message.trail with
  | None -> return ()
  | Some msg ->
    let keys = extract_pullreqs msg in
    List.map reply_of_key keys |>
    Lwt_list.iter_p (fun reply ->
      let channels = message.Irc_message.params in
      Lwt_list.iter_p (fun c ->
        Irc.send_privmsg ~connection ~target:c ~message:reply
      ) channels
    )
