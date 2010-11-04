-module(test_handler).
-compile(export_all).
%-export([go/0]).

go() -> wssv:start(?MODULE, test_handler, []).

test_handler() ->
  receive
    {message, Data, SocketSenderPid} ->
      sender ! {sendall, Data},
      test_handler();
    _Any -> 
      io:format("test"),
      test_handler()
  end.
