module Irc = Irc_client_lwt.Client

module type S = sig
  val name : string
  val rule : Irc_message.t -> bool
  val run : connection:Irc.connection_t -> message:Irc_message.t -> unit
end
