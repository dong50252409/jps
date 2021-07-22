%%%-------------------------------------------------------------------
%%% @author gz1417
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 工具函数
%%% @end
%%% Created : 21. 7月 2021 17:40
%%%-------------------------------------------------------------------
-module(jps_util).
-include("jps.hrl").

%% API
-export([g/2, h/2, get_direction/2, is_open/2]).

-spec g(Grid1 :: jps:grid(), Grid2 :: jps:grid()) -> G :: number().
g({X1, Y1}, {X2, Y2}) ->
    X3 = erlang:abs(X1 - X2),
    Y3 = erlang:abs(Y1 - Y2),
    ?IF(X3 > Y3, 14 * Y3 + 10 * (X3 - Y3), 14 * X3 + 10 * (Y3 - X3)).

-spec h(Grid1 :: jps:grid(), Grid2 :: jps:grid()) -> H :: number().
h({X1, Y1}, {X2, Y2}) ->
    (erlang:abs(X1 - X2) + erlang:abs(Y1 - Y2)) * 10.

-spec get_direction(Gird :: jps:grid(), ParentGrid :: jps:grid()) -> jps:direction().
get_direction({X1, Y1}, {X2, Y2}) ->
    {get_direction_1(X1, X2), get_direction_1(Y1, Y2)}.

get_direction_1(P1, P2) when P1 > P2 ->
    1;
get_direction_1(P1, P2) when P1 < P2 ->
    -1;
get_direction_1(P1, P1) ->
    0.

-spec is_open(Grid :: jps:grid(), VisitedGrids :: jps:visited_grids()) -> boolean().
is_open(Grid, VisitedGrids) ->
    maps:get(Grid, VisitedGrids, 0) > -1.