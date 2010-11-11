-module(websocket_server).
-import(handshake, [handshake/1]).
-export([start/0, start/5, default_echo_handler/0, unicast/2, broadcast/2, sendall/1]).
%-compile(export_all).

start() -> start("localhost", 9000, ?MODULE, default_echo_handler, []).
start(Host, Port, Module, Handler, Args) -> 
  {ok, IPaddress} = inet:getaddr(Host, inet),
  {ok, ListenSocket} = gen_tcp:listen(Port, [{ip, IPaddress}, {packet, 0}, {reuseaddr, true}, {keepalive, false}, {active, false}]),
  register(receiver, spawn(fun() -> receiver_loop() end)),
  register(sender, spawn(fun() -> sender_loop([]) end)),
  register(handler, spawn(Module, Handler, Args)),
  accept_connect_loop(ListenSocket).

receiver_loop() ->
  receive 
    {data, Frame, SocketSenderPid} ->
      handler ! {message, decode_frame(Frame), integer_to_list(erlang:phash2(SocketSenderPid))},
      receiver_loop();
    {open, SocketSenderPid} ->
      handler ! {open, integer_to_list(erlang:phash2(SocketSenderPid))},
      receiver_loop();
    {closed, SocketSenderPid} ->
      handler ! {closed, integer_to_list(erlang:phash2(SocketSenderPid))},
      receiver_loop();
    {error, PosixReason, SocketSenderPid} ->
      handler ! {error, PosixReason, integer_to_list(erlang:phash2(SocketSenderPid))},
      receiver_loop();
    _Any ->
      receiver_loop()
  end.

default_echo_handler() ->
  receive
    {message, Data, ConnectionID} -> 
      unicast(Data, ConnectionID),
      default_echo_handler();
    _Any -> 
      default_echo_handler()
  end.

unicast(Data, ConnectionID) ->
  sender ! {unicast, Data, ConnectionID}.

broadcast(Data, ConnectionID) ->
  sender ! {broadcast, Data, ConnectionID}.

sendall(Data) ->
  sender ! {sendall, Data}.

sender_loop(ConnectionIDPidList) ->
  receive
    {open, SocketSenderPid} ->
      sender_loop([{integer_to_list(erlang:phash2(SocketSenderPid)), SocketSenderPid}|ConnectionIDPidList]);
    {unicast, Data, ConnectionID} ->
      {_, SocketSenderPid} = lists:keyfind(ConnectionID, 1, ConnectionIDPidList),
      SocketSenderPid ! {send, Data},
      sender_loop(ConnectionIDPidList);
    {broadcast, Data, ConnectionID} ->
      SocketSenderPidList = lists:map(fun(X) -> element(2, X) end, lists:keydelete(ConnectionID, 1, ConnectionIDPidList)),
      sendall(Data, SocketSenderPidList),
      sender_loop(ConnectionIDPidList);
    {sendall, Data} ->
      SocketSenderPidList = lists:map(fun(X) -> element(2, X) end, ConnectionIDPidList),
      sendall(Data, SocketSenderPidList),
      sender_loop(ConnectionIDPidList);
    {closed, SocketSenderPid} ->
      sender_loop(lists:keydelete(SocketSenderPid, 2,ConnectionIDPidList));
    _Any -> 
      sender_loop(ConnectionIDPidList)
  end.

sendall(_Data, []) -> ok;
sendall(Data, [H|T]) ->
  H ! {send, Data},
  sendall(Data, T).


accept_connect_loop(ListenSocket) ->
  {ok, Socket} = gen_tcp:accept(ListenSocket),
  spawn(fun() ->  init(Socket) end),
  accept_connect_loop(ListenSocket).

init(Socket) ->
  %% Set to http packet here to do handshake
  inet:setopts(Socket, [{packet, http}]),
  ok = handshake(Socket),
  inet:setopts(Socket, [list, {packet, raw}, {active, false}]),
  SocketSenderPid = spawn(fun() -> socket_sender_loop(Socket) end),
  spawn(fun() -> socket_receiver_loop(Socket, SocketSenderPid) end),
  sender ! {open, SocketSenderPid},
  receiver ! {open, SocketSenderPid}.

socket_receiver_loop(Socket, SocketSenderPid) ->
  case gen_tcp:recv(Socket, 0) of 
    {ok, Frame} ->
      receiver ! {data, Frame, SocketSenderPid},
      socket_receiver_loop(Socket, SocketSenderPid);
    {error, closed} ->
      sender ! {closed, SocketSenderPid},
      receiver ! {closed, SocketSenderPid},
      SocketSenderPid ! closed,
      gen_tcp:close(Socket);
    {error, PosixReason} ->
      sender ! {error, PosixReason, SocketSenderPid},
      receiver ! {error, PosixReason, SocketSenderPid},
      SocketSenderPid ! {error, PosixReason},
      gen_tcp:close(Socket);
    _Any -> 
      exit(normal)    
  end. 

socket_sender_loop(Socket) -> 
  receive
    {send, Data} ->
      gen_tcp:send(Socket, [0] ++ Data ++ [255]),
      socket_sender_loop(Socket);
    closed ->
      exit(normal);
    {error, PosixReason} ->
      exit(PosixReason); 
    _Any -> 
      socket_sender_loop(Socket)
  end.

decode_frame([0|T]) -> decode_frame1(T);
decode_frame(_Any) -> []. 
decode_frame1([255]) -> [];
decode_frame1([H|T]) -> [H|decode_frame1(T)];
decode_frame1(_Any) -> [].
