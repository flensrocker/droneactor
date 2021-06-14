-module(field).

-behaviour(gen_server).

-export([start_link/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2]).
-export([add_queen/2]).

start_link(GamePid, FieldId) ->
    State = #{game_pid => GamePid, field_id => FieldId},
    gen_server:start_link(?MODULE, State, []).

stop(FieldPid) ->
    gen_server:stop(FieldPid).

add_queen(FieldPid, Queen) ->
    gen_server:call(FieldPid, {queen, Queen}).

init(State) ->
    {ok, State}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.
