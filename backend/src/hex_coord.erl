-module(hex_coord).

-export([dist/2, len/1, corner_coord/2]).

-include("game_config.hrl").

dist({AQ, AR}, {BQ, BR}) ->
    (erlang:abs(AQ - BQ) + erlang:abs(AQ + AR - BQ - BR) + erlang:abs(AR - BR)) / 2.

len({Q, R}) ->
    dist({Q, R}, {0, 0}).

corner_coord(Size, Index) when 0 < Size, 0 =< Index, Index < ?max_player_count ->
    StartFields =
        #{0 => {0, -Size},
          1 => {-Size, Size},
          2 => {Size, 0},
          3 => {-Size, 0},
          4 => {0, Size},
          5 => {Size, -Size}},
    maps:get(Index, StartFields).
