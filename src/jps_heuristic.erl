%%%-------------------------------------------------------------------
%%% @author dy
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 启发函数
%%% @end
%%% Created : 25. 7月 2021 18:06
%%%-------------------------------------------------------------------
-module(jps_heuristic).

-include("jps.hrl").

%% API
-export([manhattan/2, euclidean/2, octile/2, chebyshev/2]).

manhattan({X1, Y1}, {X2, Y2}) ->
    erlang:abs(X1 - X2) + erlang:abs(Y1 - Y2).

euclidean({X1, Y1}, {X2, Y2}) ->
    DX = erlang:abs(X1 - X2),
    DY = erlang:abs(Y1 - Y2),
    math:sqrt(DX * DX + DY * DY).

octile({X1, Y1}, {X2, Y2}) ->
    F = 0.4142135,
    DX = erlang:abs(X1 - X2),
    DY = erlang:abs(Y1 - Y2),
    F * erlang:min(DX, DY) + erlang:max(DX, DY).

chebyshev({X1, Y1}, {X2, Y2}) ->
    DX = erlang:abs(X1 - X2),
    DY = erlang:abs(Y1 - Y2),
    erlang:max(DX, DY).