-module(canvas_sharing_handler).
-import(websocket_server, [unicast/2, broadcast/2, sendall/1]).
-compile(export_all).

go() -> websocket_server:start("etsukata.com", 9000, ?MODULE, canvas_sharing_handler, [[],[]]).

canvas_sharing_handler(IDList, PointList) ->
  receive
    {open, ConnectionID} ->
      broadcast("@NC:" ++ ConnectionID, ConnectionID),
      unicast("@ID:" ++ ConnectionID, ConnectionID),
      StrIDList = lists:foldr(fun(X, Xs) -> X ++ "," ++ Xs end, "", IDList),
      Str = case StrIDList of 
              []   -> [];
              _Any -> lists:sublist(StrIDList, length(StrIDList) -1)
            end,
      unicast("@PT:" ++ Str, ConnectionID),
      lists:foreach(fun(X) -> unicast(X, ConnectionID) end, lists:reverse(PointList)),
      canvas_sharing_handler([ConnectionID|IDList], PointList);
    {message, Data, ConnectionID} ->
      case lists:member($@, Data) of 
        true -> 
          case string:substr(Data, 1, 3) of 
            "@CU" ->
              broadcast(Data, ConnectionID),
              canvas_sharing_handler(IDList, []);
            _Any ->
              canvas_sharing_handler(IDList, PointList)
          end;
        false ->
          broadcast(Data, ConnectionID),
          canvas_sharing_handler(IDList, [Data|PointList])
      end;
    {closed, ConnectionID} ->
      broadcast("@CL:" ++ ConnectionID, ConnectionID),
      canvas_sharing_handler(lists:delete(ConnectionID,IDList), PointList);
    Any ->
      io:format("Any:~w~n", [Any]),     
      canvas_sharing_handler(IDList, PointList)
  end.
