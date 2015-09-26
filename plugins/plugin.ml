module Irc = Irc_client_lwt

module type S = sig
  val name : string
  val rule : Irc_message.t -> bool
  val run : connection:Irc.connection_t -> message:Irc_message.t -> unit Lwt.t
end
