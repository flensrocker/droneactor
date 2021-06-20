-module(player_register_handler).

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
    GameName = maps:get(<<"game_name">>, ReqData),
    PlayerName = maps:get(<<"player_name">>, ReqData),
    PlayerId =
        list_to_binary(uuid:to_string(
                           uuid:uuid4())),
    % TODO use jwt for cookie-content
    Req2 = cowboy_req:set_resp_cookie(<<"player_id">>, PlayerId, Req1, #{path => "/api"}),
    Req3 = cowboy_req:set_resp_cookie(<<"player_name">>, PlayerName, Req2, #{path => "/api"}),
    Req4 = cowboy_req:set_resp_cookie(<<"game_name">>, GameName, Req3, #{path => "/api"}),
    Reply =
        #{<<"player_id">> => PlayerId,
          <<"player_name">> => PlayerName,
          <<"game_name">> => GameName},
    Req5 =
        cowboy_req:reply(200,
                         #{<<"content-type">> => <<"application/json">>},
                         jsx:encode(Reply),
                         Req4),
    {ok, Req5, State};
init(Req0, State) ->
    Req1 = cowboy_req:reply(405, #{<<"allow">> => <<"POST">>}, Req0),
    {ok, Req1, State}.
