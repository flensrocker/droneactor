-module(queen).

-behaviour(gen_server).

-export([start_link/3, stop/1]).
-export([init/1, handle_call/3, handle_cast/2]).

start_link(GamePid, PlayerPid, FieldId = {_Q, _R}) ->
    State = #{game_pid => GamePid, player_pid => PlayerPid, field_id => FieldId},
    gen_server:start_link(?MODULE, State, []).

stop(QueenPid) ->
    gen_server:stop(QueenPid).

init(State) ->
    {ok, State}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.
