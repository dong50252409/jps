%%%-------------------------------------------------------------------
%%% @author gz1417
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 一些定义
%%% @end
%%% Created : 19. 7月 2021 11:48
%%%-------------------------------------------------------------------
-ifndef(JPS_HRL).
-define(JPS_HRL, true).

-define(IF(Condition, T, F), (
    case Condition of
        true ->
            T;
        false ->
            F
    end
)).

-define(DIRECTIONS(X1, Y1, X2, Y2), (
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
    end
)).

-endif.