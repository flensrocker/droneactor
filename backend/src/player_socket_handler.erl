-module(player_socket_handler).

-behaviour(gen_event).
-behaviour(cowboy_websocket).

-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2]).
-export([init/1, handle_event/2, handle_call/2]).
-export([terminate/2]).

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
    {JoinState, #{game_pid := GamePid, event_pid := EventPid}} =
        game_registry:join_game(JoinRequest),
    EventRef = erlang:make_ref(),
    State =
        maps:merge(JoinRequest,
                   #{game_pid => GamePid,
                     event_ref => EventRef,
                     join_state => JoinState}),
    gen_event:add_handler(EventPid, {?MODULE, EventRef}, #{socket_pid => erlang:self()}),
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

websocket_info(Event = #{<<"message">> := _Msg, <<"payload">> := _Payload}, State) ->
    {[{text, jsx:encode(Event)}], State};
websocket_info(_Info, State) ->
    {ok, State}.

init(EventState) ->
    {ok, EventState}.

handle_event(Event, EventState = #{socket_pid := SocketPid}) ->
    SocketPid ! Event,
    {ok, EventState}.

handle_call(_Event, EventState) ->
    {ok, _Event, EventState}.

terminate(_Reason, State = #{event_pid := EventPid, event_ref := EventRef}) ->
    gen_event:delete_handler(EventPid, {?MODULE, EventRef}, State),
    ok;
terminate(Reason, _EventState = #{socket_pid := SocketPid}) ->
    SocketPid ! Reason,
    ok.
