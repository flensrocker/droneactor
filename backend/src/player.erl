-module(player).

-behaviour(gen_server).

-export([start_link/5, stop/1]).
-export([init/1, handle_call/3, handle_cast/2]).

start_link(GamePid, EventPid, PlayerId, PlayerName, QueenField = {_Q, _R}) ->
    State =
        #{game_pid => GamePid,
          event_pid => EventPid,
          player_id => PlayerId,
          player_name => PlayerName,
          queen_field => QueenField},
    gen_server:start_link(?MODULE, State, []).

stop(PlayerPid) ->
    gen_server:stop(PlayerPid).

init(State =
         #{game_pid := GamePid,
           event_pid := EventPid,
           player_id := PlayerId,
           queen_field := QueenField}) ->
    {ok, QueenPid} = queen:start_link(GamePid, EventPid, erlang:self(), PlayerId, QueenField),
    State1 = maps:put(queen_pid, QueenPid, State),
    {ok, State1}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.
