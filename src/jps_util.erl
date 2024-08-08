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
    if
        X1 > X2 ->
            if 
                Y1 > Y2 -> {1, 1};
                Y1 < Y2 -> {1, -1};
                true -> {1, 0}
            end;
        X1 < X2 ->
            if
                Y1 > Y2 -> {-1, 1};
                Y1 < Y2 -> {-1, -1};
                true -> {-1, 0}
            end;
        true ->
            if
                Y1 > Y2 -> {0, 1};
                Y1 < Y2 -> {0, -1};
                true -> {0, 0}
            end
    end.