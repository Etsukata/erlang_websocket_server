Erlang_WebSocket_Server
=======================

Erlang_WebSocket_Server is a server-side implementation for the WebSocket Protocol written in Erlang.
This library conforms to WebSocket [draft-76][1].The library provides efficient ways of developoing systems which use WebSocket.
Handshake process mainly uses the code written by davebryson and ENDOH Takanao.
davebryson:[https://github.com/davebryson/erlang_websocket][2]
ENDOH Takano:[https://github.com/MiCHiLU/erlang_websocket][3]

Process Design Pattern
----------------------

This library has the following process design pattern.

FIGURE

Each socket sender and reciever are connected to a corresponding WebSocket cliend. A socket receiver receives WebSocket frames or error message and pass them to the receiver process. The receiver decode passed frames. The handler handles unframed data and errors. The sender frames handled data and sends them to the specific socket sender or broadcast to all except for the socket sender connected to source client. The sender can also send data to all socket senders.
All you have to write is what the handler does.

Simple Echo Server 
------------------
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


General Case
------------

Demonstration
-------------

Getting Started
---------------

[1]:http://tools.ietf.org/html/draft-hixie-thewebsocketprotocol-76
[2]:https://github.com/davebryson/erlang_websocket
[3]:https://github.com/MiCHiLU/erlang_websocket
