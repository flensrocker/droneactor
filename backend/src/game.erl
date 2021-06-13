-module(game).

-behaviour(gen_server).

-export([start_link/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2]).
-export([join_player/2, get_state/1]).

start_link(Name, Size) ->
    State =
        #{name => Name,
          size => Size,
          fields => #{},
          players => #{},
          player_pids => #{}},
    gen_server:start_link(?MODULE, State, []).

stop(GamePid) ->
    gen_server:stop(GamePid).

join_player(GamePid, Player = #{player_id := _PlayerId, player_name := _PlayerName}) ->
    gen_server:call(GamePid, {join, Player}).

get_state(GamePid) ->
    gen_server:call(GamePid, state).

init(State = #{size := Size}) ->
    C = lists:seq(-Size, Size),
    FieldList = [{{Q, R}, #{}} || Q <- C, R <- C, hex_coord:len({Q, R}) =< Size],
    Fields = maps:from_list(FieldList),
    State1 = maps:put(fields, Fields, State),
    {ok, State1}.

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
                {Pid, S2};
            true ->
                Pid = maps:get(PlayerId, PlayerPids),
                {Pid, State}
        end,
    {reply, {ok, PlayerPid}, State1};
handle_call(state,
            _From,
            State =
                #{name := GameName,
                  size := GameSize,
                  fields := FieldMap,
                  players := PlayerMap}) ->
    FieldList = lists:map(fun(F) -> map_field(F) end, maps:to_list(FieldMap)),
    PlayerList = lists:map(fun(P) -> map_player(P) end, maps:to_list(PlayerMap)),
    GameState =
        #{<<"game_name">> => GameName,
          <<"game_size">> => GameSize,
          <<"fields">> => FieldList,
          <<"players">> => PlayerList},
    {reply, {ok, GameState}, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

map_field({{Q, R}, _Field}) ->
    #{<<"coord_q">> => Q, <<"coord_r">> => R}.

map_player({_PlayerId, #{player_name := PlayerName}}) ->
    #{<<"player_name">> => PlayerName}.
