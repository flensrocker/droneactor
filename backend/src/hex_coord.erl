-module(hex_coord).

-export([dist/2, len/1]).

dist({AQ, AR}, {BQ, BR}) ->
    (erlang:abs(AQ - BQ) + erlang:abs(AQ + AR - BQ - BR) + erlang:abs(AR - BR)) / 2.

len({Q, R}) ->
    dist({Q, R}, {0, 0}).
