-module(echo_handler).
-compile(export_all).
-import(websocket_server, [unicast/2]).

go() ->
   websocket_server:start("localhost", 9000, ?MODULE, default_echo_handler, []).

default_echo_handler() ->
  receive
    {message, Data, ConnectionID} -> 
      unicast(Data, ConnectionID),
      default_echo_handler();
    _Any -> default_echo_handler()
  end.
