-module(user_register_handler).

-behaviour(cowboy_handler).

-export([init/2]).

init(Req0 = #{method := <<"OPTIONS">>}, State) ->
    Req1 =
        cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"GET, OPTIONS">>, Req0),
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req1),
    {ok, Req2, State};
init(Req0 = #{method := <<"POST">>}, State) ->
    {ok, ReqBody, Req1} = cowboy_req:read_body(Req0),
    ReqData = jsx:decode(ReqBody, [return_maps]),
    Game = maps:get(<<"game">>, ReqData),
    Username = maps:get(<<"username">>, ReqData),
    UserId =
        list_to_binary(uuid:to_string(
                           uuid:uuid4())),
    Req2 = cowboy_req:set_resp_cookie(<<"userid">>, UserId, Req1, #{path => "/api"}),
    Req3 = cowboy_req:set_resp_cookie(<<"username">>, Username, Req2, #{path => "/api"}),
    Req4 = cowboy_req:set_resp_cookie(<<"game">>, Game, Req3, #{path => "/api"}),
    Reply =
        #{<<"userId">> => UserId,
          <<"game">> => Game,
          <<"username">> => Username},
    Req5 =
        cowboy_req:reply(200,
                         #{<<"content-type">> => <<"application/json">>},
                         jsx:encode(Reply),
                         Req4),
    {ok, Req5, State};
init(Req0, State) ->
    Req1 = cowboy_req:reply(405, #{<<"allow">> => <<"POST">>}, Req0),
    {ok, Req1, State}.
