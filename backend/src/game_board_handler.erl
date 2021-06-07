-module(game_board_handler).

-behaviour(cowboy_websocket).

-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2]).

init(Req0 = #{method := <<"OPTIONS">>}, State) ->
    Req1 =
        cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"GET, OPTIONS">>, Req0),
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req1),
    {ok, Req2, State};
init(Req0, _) ->
    UserGame = cowboy_req:match_cookies([userid, username, game], Req0),
    {cowboy_websocket, Req0, UserGame}.
websocket_init(State) ->
	{[{text, <<"{\"type\":\"hello\"}">>}], State}.
websocket_handle(Frame = {text, _}, State) ->
	{[Frame], State};
websocket_handle(_Frame, State) ->
	{ok, State}.
websocket_info(_Info, State) ->
	{ok, State}.
