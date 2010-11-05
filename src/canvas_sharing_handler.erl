-module(canvas_sharing_handler).
-compile(export_all).
%-export([go/0]).

go() -> websocket_server:start("etsukata.com", 9000, ?MODULE, test_handler, [[],[]]).

test_handler(IDList, PointList) ->
  receive
    {open, SocketSenderPid} ->
      ID = integer_to_list(erlang:phash2(SocketSenderPid)),
      sender ! {broadcast, "@NC:" ++ ID, SocketSenderPid},
      sender ! {unicast, "@ID:" ++ ID, SocketSenderPid},
      sender ! {unicast, "@PT:" ++ ID, SocketSenderPid},
      lists:foreach(fun(X) -> sender ! {unicast, X, SocketSenderPid} end, lists:reverse(PointList)),
      test_handler([ID|IDList], PointList);
    {message, Data, SocketSenderPid} ->
      case lists:member($@, Data) of 
        true -> 
          case string:substr(Data, 1, 3) of 
            "@CU" ->
              sender ! {broadcast, Data, SocketSenderPid},
              test_handler(IDList, []);
            _Any ->
              test_handler(IDList, PointList)
          end;
        false ->
          sender ! {broadcast, Data, SocketSenderPid},
          test_handler(IDList, [Data|PointList])
      end;
    {closed, SocketSenderPid} ->
      ID = integer_to_list(erlang:phash2(SocketSenderPid)), 
      sender ! {broadcast, "@CL:" ++ ID, SocketSenderPid},
      test_handler(lists:delete(ID,IDList), PointList);
    Any ->
io:format("Any:~w~n", [Any]),     
 test_handler(IDList, PointList)
  end.
