-module(player).

-behaviour(gen_server).

-export([start_link/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2]).

start_link(PlayerId, PlayerName) ->
    State = #{player_id => PlayerId, player_name => PlayerName},
    gen_server:start_link(?MODULE, State, []).

stop(PlayerPid) ->
    gen_server:stop(PlayerPid).

init(State) ->
    {ok, State}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.
