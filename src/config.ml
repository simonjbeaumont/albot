let of_file path =
  try Ag_util.Json.from_file Config_j.read_config path
  with Yojson.Json_error s ->
    failwith ("Error loading config file: " ^ s)

let string_of t = Yojson.Safe.prettify ~std:true (Config_j.string_of_config t)
