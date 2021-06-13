-module(droneactor_app).

-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    game_registry:start_link(),
    Dispatch =
        cowboy_router:compile([{'_',
                                [{"/api/user/register", user_register_handler, #{}},
                                 {"/api/game/board", game_board_handler, #{}}]}]),
    {ok, _} =
        cowboy:start_clear(drone_http_handler, [{port, 8080}], #{env => #{dispatch => Dispatch}}),
    droneactor_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(drone_http_handler),
    game_registry:stop().
