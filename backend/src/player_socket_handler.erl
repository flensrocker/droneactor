-module(player_socket_handler).

-behaviour(cowboy_websocket).

-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2]).

init(Req0 = #{method := <<"OPTIONS">>}, State) ->
    Req1 =
        cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"GET, OPTIONS">>, Req0),
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req1),
    {ok, Req2, State};
init(Req0, _) ->
    JoinRequest = cowboy_req:match_cookies([player_id, player_name, game_name], Req0),
    {cowboy_websocket, Req0, JoinRequest}.

websocket_init(JoinRequest =
                   #{player_id := _PlayerId,
                     player_name := _PlayerName,
                     game_name := _GameName}) ->
    {JoinState, #{game_pid := GamePid}} = game_registry:join_game(JoinRequest),
    State = maps:merge(JoinRequest, #{game_pid => GamePid, join_state => JoinState}),
    {ok, GameState} = game:get_state(GamePid),
    GameState1 =
        case JoinState of
            ok ->
                maps:put(<<"join_state">>, <<"player">>, GameState);
            game_full ->
                maps:put(<<"join_state">>, <<"viewer">>, GameState)
        end,
    {[{text, jsx:encode(#{<<"message">> => <<"game_state">>, <<"payload">> => GameState1})}],
     State}.

websocket_handle(Frame = {text, _}, State) ->
    {reply, [Frame], State};
websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info(_Info, State) ->
    {ok, State}.
