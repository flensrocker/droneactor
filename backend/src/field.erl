-module(field).

-behaviour(gen_server).

-export([start_link/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2]).

start_link(FieldId, GamePid) ->
    State = #{field_id => FieldId, game_pid => GamePid},
    gen_server:start_link(?MODULE, State, []).

stop(FieldPid) ->
    gen_server:stop(FieldPid).

init(State) ->
    {ok, State}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.
