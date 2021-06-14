-module(game).

-behaviour(gen_server).

-export([start_link/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2]).
-export([join_player/2, get_state/1]).

-include("game_config.hrl").

start_link(Name, Size) when 0 < Size, Size =< ?max_game_size ->
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
    FieldIds = [{Q, R} || Q <- C, R <- C, hex_coord:len({Q, R}) =< Size],
    Fields =
        maps:from_list(
            lists:map(fun(FieldId) -> {FieldId, #{}} end, FieldIds)),
    FieldPids =
        maps:from_list(
            lists:map(fun(FieldId) ->
                         {ok, FieldPid} =
                             field:start_link(
                                 erlang:self(), FieldId),
                         {FieldId, FieldPid}
                      end,
                      FieldIds)),
    State1 = maps:merge(State, #{fields => Fields, field_pids => FieldPids}),
    {ok, State1}.

handle_call({join, Player = #{player_id := PlayerId, player_name := PlayerName}},
            _From,
            State =
                #{players := Players,
                  player_pids := PlayerPids,
                  size := GameSize}) ->
    PlayerCount = maps:size(Players),
    {Reply, State1} =
        case maps:is_key(PlayerId, Players) of
            false when PlayerCount =:= ?max_player_count ->
                {game_full, State};
            false ->
                QueenField = hex_coord:corner_coord(GameSize, PlayerCount),
                {ok, PlayerPid} =
                    player:start_link(
                        erlang:self(), PlayerId, PlayerName, QueenField),
                Players1 = maps:put(PlayerId, Player, Players),
                PlayerPids1 = maps:put(PlayerId, PlayerPid, PlayerPids),
                S1 = maps:merge(State, #{players => Players1, player_pids => PlayerPids1}),
                {{ok, PlayerPid}, S1};
            true ->
                PlayerPid = maps:get(PlayerId, PlayerPids),
                {{ok, PlayerPid}, State}
        end,
    {reply, Reply, State1};
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
