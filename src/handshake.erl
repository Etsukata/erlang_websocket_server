-module(handshake).
%-export([handshake/1]).
-compile(export_all).
-define(WEBSOCKET_PREFIX,"HTTP/1.1 101 Web Socket Protocol Handshake\r\nUpgrade: WebSocket\r\nConnection: Upgrade\r\n").
-define(HEX, [{"0", 0}, {"1", 1}, {"2", 2}, {"3", 3}, {"4", 4}, {"5", 5}, {"6", 6}, {"7", 7}, {"8", 8}, {"9", 9},
              {"a", 10}, {"b", 11}, {"c", 12}, {"d", 13}, {"e", 14}, {"f", 15}]).

handshake(Socket) ->
    case gen_tcp:recv(Socket, 0) of
	{ok, {http_request, _Method, Path, _Version}} ->
	    {abs_path,PathA} = Path,
	    check_header(Socket,PathA,[]);
	{error, {http_error, "\r\n"}} ->
            handshake(Socket);
        {error, {http_error, "\n"}} ->
            handshake(Socket);
        Other ->
	    io:format("Got: ~p~n",[Other]),
            gen_tcp:close(Socket),
            exit(normal)
    end.

check_header(Socket,Path,Headers) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, http_eoh} ->
	    verify_handshake(Socket,Path,Headers);
        {ok, {http_header, _, Name, _, Value}} ->
            check_header(Socket, Path, [{Name, Value} | Headers]);
        _Other ->
            gen_tcp:close(Socket),
            exit(normal)
    end.

verify_handshake(Socket,Path,Headers) ->
    %error_logger:info_msg("Incoming Headers: ~p~n",[Headers]),
    case proplists:get_value('Upgrade',Headers) of
	"WebSocket" ->
	    send_handshake(Socket,Path,Headers);
	_Other ->
	    error_logger:error_msg("Incorrect WebSocket headers. Closing the connection~n"),
	    gen_tcp:close(Socket),
            exit(normal)
    end.

get_digit_space(String) ->
    char_filter([], [], String).
char_filter(Digits, Spaces, []) ->
    {list_to_integer(Digits), length(Spaces)};
char_filter(Digits, Spaces, [H|T]) ->
    try _ = list_to_integer([H]),
        char_filter(Digits ++ [H], Spaces, T)
    catch error:_ ->
        case H of
            32  ->
                char_filter(Digits, Spaces ++ [H], T);
            _   ->
                char_filter(Digits, Spaces, T)
        end
    end.

make_challenge(Part_1, Part_2, Key_3) ->
    %Sample = to_32bit_int(lists:flatten(io_lib:format("~8.16.0b~8.16.0b", [906585445, 179922739]))) ++ "WjN}|M(6",
    %error_logger:info_msg("Sample: ~p~n", [{Sample, erlang:md5(Sample)}]),
    to_32bit_int(lists:flatten(io_lib:format("~8.16.0b~8.16.0b", [Part_1, Part_2]))) ++ binary_to_list(Key_3).
to_32bit_int(Parts) ->
    %error_logger:info_msg("to_32bit_int:Parts: ~p~n", [{Parts}]),
    to_32bit_int([], Parts).
to_32bit_int(Result, []) ->
    Result;
to_32bit_int(Result, [H1,H2|T]) ->
    {_, H1_} = lists:keyfind([H1], 1, ?HEX),
    {_, H2_} = lists:keyfind([H2], 1, ?HEX),
    %error_logger:info_msg("to_32bit_int:H1,H2: ~p~n", [{H1_, H2_}]),
    to_32bit_int(Result ++ [H1_*16+H2_], T).

send_handshake(Socket,Path,Headers) ->
    Origin = proplists:get_value("Origin",Headers),
    Location = proplists:get_value('Host',Headers),
    %draft-76: 5.2.2
    Key_1 = proplists:get_value("Sec-Websocket-Key1",Headers),
    Key_2 = proplists:get_value("Sec-Websocket-Key2",Headers),
    %draft-76: 5.2.4
    %draft-76: 5.2.5
    {Key_number_1, Spaces_1} = get_digit_space(Key_1),
    {Key_number_2, Spaces_2} = get_digit_space(Key_2),
    %draft-76: 5.2.6
    0 = Key_number_1 rem Spaces_1,
    0 = Key_number_2 rem Spaces_2,
    %draft-76: 5.2.7
    Part_1 = Key_number_1 div Spaces_1,
    Part_2 = Key_number_2 div Spaces_2,
    %draft-76: 5.2.8
    inet:setopts(Socket, [binary, {packet, raw}]),
    {ok, Key_3} = gen_tcp:recv(Socket, 0),
    Challenge = make_challenge(Part_1, Part_2, Key_3),
    %draft-76: 5.2.9
    Response = erlang:md5(Challenge),
    %draft-76: 5.2.10
    Resp = ?WEBSOCKET_PREFIX ++
    %draft-76: 5.2.11
    "Sec-WebSocket-Origin: " ++ Origin ++ "\r\n" ++
    "Sec-WebSocket-Location: ws://" ++ Location ++ Path ++
    %draft-76: 5.2.12
    "\r\n\r\n" ++
    %draft-76: 5.2.13
    Response,
    %error_logger:info_msg("Sec-Websocket-Keys: ~p~n", [
    %        [{"key_1", Key_1}, {"key_2", Key_2}, {"key_3", Key_3},
    %         {"key-number_1", Key_number_1}, {"key-number_2", Key_number_2},
    %         {"spaces_1", Spaces_1}, {"spaces_2", Spaces_2},
    %         {"part_1", Part_1}, {"part_2", Part_2},
    %         {"challenge", Challenge}, {"response", Response, binary_to_list(Response)}]]),
    gen_tcp:send(Socket, Resp),
    ok.

