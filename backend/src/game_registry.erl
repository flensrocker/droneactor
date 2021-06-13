-module(game_registry).

-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2]).
-export([join_game/1]).

start_link() ->
    gen_server:start_link({local, game_registry}, ?MODULE, #{games => #{}}, []).

stop() ->
    gen_server:stop(game_registry).

join_game(Args =
              #{player_id := _PlayerId,
                player_name := _PlayerName,
                game_name := _GameName}) ->
    gen_server:call(game_registry, {join, Args});
join_game(_Args) ->
    badargs.

init(State) ->
    %% trap exit?
    {ok, State}.

handle_call({join,
             #{player_id := PlayerId,
               player_name := PlayerName,
               game_name := GameName}},
            _From,
            State = #{games := Games}) ->
    Game = maps:get(GameName, Games, #{name => GameName, size => 3}),
    {GamePid, State1} =
        case maps:is_key(pid, Game) of
            false ->
                {ok, Pid} = game:start_link(GameName, maps:get(size, Game)),
                G = maps:put(pid, Pid, Game),
                Gs = maps:put(GameName, G, Games),
                S = maps:put(games, Gs, State),
                {Pid, S};
            true ->
                Pid = maps:get(pid, Game),
                {Pid, State}
        end,
    game:join_player(GamePid, #{player_id => PlayerId, player_name => PlayerName}),
    Reply = #{game_pid => GamePid},
    {reply, {ok, Reply}, State1};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.
