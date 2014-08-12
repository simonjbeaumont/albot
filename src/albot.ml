module Irc = Irc_client_lwt.Client
open Lwt

let available_plugins = [(module Logger : Plugin.S); (module Jira)]
let active_plugins = ref []

let string_of_message {Irc_message.prefix; command; params; trail} =
  let string_of_string_opt = function Some s -> s | None -> "None" in
  let string_of_string_list l = Printf.sprintf "[%s]" (String.concat "; " l) in
  Printf.sprintf "%s %s %s %s" (string_of_string_opt prefix) command
    (string_of_string_list params) (string_of_string_opt trail)

let callback ~connection ~result =
  let open Irc_message in
  match result with
  | Message contents ->
    Lwt_io.printlf "Received a message: %s" (string_of_message contents) >>= fun () ->
    List.filter (fun (module P : Plugin.S) -> P.rule contents) !active_plugins |>
    Lwt_list.iter_p
      (fun (module P : Plugin.S) ->
        Lwt_io.printlf "Firing plugin %s" P.name >>= fun () ->
        P.run ~connection ~message:contents)
  | Parse_error (message, error) ->
    Lwt_io.printlf "Couldn't parse \"%s\": %s" message error

let dump_conf c = Lwt_io.printlf "Running with config: %s" (Config.string_of c)

let load_plugin p =
  let module P = (val p : Plugin.S) in
  Lwt_io.printlf "Loaded plugin %s" P.name >>= fun () ->
  return (active_plugins := p :: !active_plugins)

let start config =
  dump_conf config >>= fun () ->
  let plugins = match config.Config_t.plugins with Some ps -> ps | None -> [] in
  List.filter (fun (module P : Plugin.S) -> List.mem P.name plugins) available_plugins |>
  Lwt_list.iter_s load_plugin >>= fun () ->
  let {Config_t.server; port; username; password; channel; nick; realname} =
    config
  in
  Irc.connect_by_name ~server ~port ~username ?password ~nick ~realname
    ~mode:0 () >>= function
  | None -> Lwt.fail (Failure "Host not found")
  | Some connection ->
    Lwt_io.printl "Connected!" >>= fun () ->
    Irc.send_join ~connection ~channel >>= fun () ->
    Irc.listen ~connection ~callback

let run config = Lwt_main.run (start config)
