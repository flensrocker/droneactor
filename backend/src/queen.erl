-module(queen).

-behaviour(gen_server).

-export([start_link/5, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-include("game_config.hrl").

start_link(GamePid, EventPid, PlayerPid, PlayerId, FieldId = {_Q, _R}) ->
    State =
        #{game_pid => GamePid,
          event_pid => EventPid,
          player_pid => PlayerPid,
          player_id => PlayerId,
          field_id => FieldId,
          next_drone_type => <<"worker">>},
    gen_server:start_link(?MODULE, State, []).

stop(QueenPid) ->
    gen_server:stop(QueenPid).

init(State) ->
    %% TODO add queen to field
    erlang:send_after(?queen_tick, erlang:self(), tick),
    {ok, State}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(tick,
            State =
                #{event_pid := EventPid,
                  player_id := PlayerId,
                  field_id := {Q, R},
                  next_drone_type := NextDroneType}) ->
    gen_event:notify(EventPid,
                     #{<<"message">> => <<"create_drone">>,
                       <<"payload">> =>
                           #{<<"field_coord_q">> => Q,
                             <<"field_coord_r">> => R,
                             <<"player_id">> => PlayerId,
                             <<"drone_type">> => NextDroneType}}),
    erlang:send_after(?queen_tick, erlang:self(), tick),
    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.
