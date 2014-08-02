let default () =
  Config_v.create_config
    ~server:"irc.freenode.net"
    ~port:6667
    ~username:"albot_"
    ?password:None
    ~channel:"#albot"
    ~nick:"albot_"
    ~realname:"Albot, the friendly OCaml IRC bot"
    ()

let string_of t = Yojson.Safe.prettify ~std:true (Config_j.string_of_config t)
