-module(game).

-behaviour(gen_server).

-export([start_link/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2]).
-export([join_player/2]).

start_link(Name, Size) ->
    State =
        #{name => Name,
          size => Size,
          players => #{},
          player_pids => #{}},
    gen_server:start_link(?MODULE, State, []).

stop(GamePid) ->
    gen_server:stop(GamePid).

join_player(GamePid, Player = #{player_id := _PlayerId, player_name := _PlayerName}) ->
    gen_server:call(GamePid, {join, Player}).

init(State) ->
    {ok, State}.

handle_call({join, Player = #{player_id := PlayerId, player_name := PlayerName}},
            _From,
            State) ->
    Players = maps:get(players, State),
    PlayerPids = maps:get(player_pids, State),
    {PlayerPid, State1} =
        case maps:is_key(PlayerId, Players) of
            false ->
                {ok, Pid} = player:start_link(PlayerId, PlayerName),
                Ps = maps:put(PlayerId, Player, Players),
                PPs = maps:put(PlayerId, Pid, PlayerPids),
                S1 = maps:put(players, Ps, State),
                S2 = maps:put(player_pids, PPs, S1),
                {x, S2};
            true ->
                {maps:get(PlayerId, PlayerPids), State}
        end,
    {reply, {ok, PlayerPid}, State1};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.
