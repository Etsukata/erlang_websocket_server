-module(echo_handler).
-compile(export_all).
go() ->
   websocket_server:start("localhost", 9000, ?MODULE, default_echo_handler, []).

default_echo_handler() ->
  receive
    {message, Data, SocketSenderPid} -> 
      sender ! {unicast, Data, SocketSenderPid},
      default_echo_handler();
    _Any -> default_echo_handler()
  end.
