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

-endif.