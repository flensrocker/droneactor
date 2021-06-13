-module(game).

-behaviour(gen_server).

-export([start_link/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2]).
-export([join_player/2]).

start_link(Name, Size) ->
    State =
        #{name => Name,
          size => Size,
          players => #{}},
    gen_server:start_link(game, State, []).

stop(GamePid) ->
    gen_server:stop(GamePid).

join_player(GamePid, Player = #{player_id := _PlayerId, player_name := _PlayerName}) ->
    gen_server:call(GamePid, {join, Player}).

init(State) ->
    {ok, State}.

handle_call({join, Player = #{player_id := PlayerId, player_name := _PlayerName}},
            _From,
            State) ->
    Players = maps:get(players, State),
    Players1 = maps:put(PlayerId, Player, Players),
    State1 = maps:put(players, Players1, State),
    {reply, ok, State1};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.
