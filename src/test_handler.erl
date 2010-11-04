-module(test_handler).
-export([go/0]).

go() -> wssv:start(fun test_handler/0).

test_handler() ->
  receive
    {message, Data, SocketSenderPid} ->
      sender ! {sendall, Data},
      test_handler();
    _Any -> 
      io:format("test"),
      test_handler()
  end.
