open Cmdliner

let config =
  let config_file =
    let doc = "Path to configuration file." in
    Arg.(required & opt (some file) None & info ["c"; "config"] ~docv:"CONFIG" ~doc)
  in
  Term.(pure Config.of_file $ config_file)

let dump_config =
  let doc = "Print the current parameters to stdout and exit." in
  Arg.(value & flag & info ["print-config"] ~doc)

let cmd = 
  let doc = "The friendly and extensible OCaml IRC bot" in
  let man = [
    `S "DESCRIPTION";
    `P "$(tname) is a bit dim at the moment! Currently, he sits in a channel
        and logs all messages to stdout.";
    `S "BUGS"; 
    `P "Bugs can be reported at http://github.com/simonjbeaumont/albot" ]
  in
  Term.(pure Albot.run $ config),
  Term.info "albot" ~version:"0.0.0" ~doc ~man

let () = match Term.eval cmd with `Error _ -> exit 1 | _ -> exit 0
