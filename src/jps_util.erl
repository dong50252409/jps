%%%-------------------------------------------------------------------
%%% @author dy
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 工具函数
%%% @end
%%% Created : 21. 7月 2021 17:40
%%%-------------------------------------------------------------------
-module(jps_util).
-include("jps.hrl").

%% API
-export([ get_direction/2]).

-spec get_direction(Gird :: jps:grid(), ParentGrid :: jps:grid()) -> jps:direction().
get_direction({X1, Y1}, {X2, Y2}) ->
    {get_direction_1(X1, X2), get_direction_1(Y1, Y2)}.

get_direction_1(P1, P2) when P1 > P2 ->
    1;
get_direction_1(P1, P2) when P1 < P2 ->
    -1;
get_direction_1(P1, P1) ->
    0.
