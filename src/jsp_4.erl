%%%-------------------------------------------------------------------
%%% @author gz1417
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. 7月 2021 11:03
%%%-------------------------------------------------------------------
-module(jsp_4).
-author("gz1417").

%% API
-export([search/3]).

-define(IF(Condition, T, F), (
    case Condition of
        true ->
            T;
        false ->
            F
    end
)).

search(StartGrid, EndGrid, ValidFun) ->
    OpenGrids = add_open_grid(0, StartGrid, 0, gb_trees:empty()),
    ClosedGirds = #{},
    ParentGrids = #{StartGrid => true},
    case do_search(EndGrid, ValidFun, OpenGrids, ClosedGirds, ParentGrids) of
        none ->
            none;
        {EndGrid, ParentGrids1} ->
            get_full_path(EndGrid, ParentGrids1)
    end.

do_search(EndGrid, ValidFun, OpenGrids, ClosedGirds, ParentGrids) ->
    case gb_trees:is_empty(OpenGrids) of
        true ->
            none;
        false ->
            case gb_trees:take_smallest(OpenGrids) of
                {{_, EndGrid}, _G, _OpenGrids1} ->
                    {EndGrid, ParentGrids};
                {{_Score, Grid}, G, OpenGrids1} ->
                    ClosedGirds1 = ClosedGirds#{Grid => true},
                    ParentGrid = maps:get(Grid, ParentGrids),
                    Directions = direction(Grid, ParentGrid, ValidFun),
                    NeighbourGrids = get_neighbour_girds(Grid, EndGrid, ValidFun, ClosedGirds1, Directions, []),
                    {OpenGrids2, ParentGrids1} = add_neighbour_girds(EndGrid, Grid, G, OpenGrids1, ParentGrids, NeighbourGrids),
                    do_search(EndGrid, ValidFun, OpenGrids2, ClosedGirds1, ParentGrids1)
            end
    end.

get_neighbour_girds(Grid, EndGrid, ValidFun, ClosedGrids, [Direction | T], NeighbourGrids) ->
    NeighbourGrids1 = get_neighbour_girds_1(Grid, EndGrid, ValidFun, ClosedGrids, Direction, NeighbourGrids),
    get_neighbour_girds(Grid, EndGrid, ValidFun, ClosedGrids, T, NeighbourGrids1);
get_neighbour_girds(_Grid, _EndGrid, _ValidFun, _ClosedGrids, [], NeighbourGrids) ->
    NeighbourGrids.

get_neighbour_girds_1({X, Y} = Grid, EndGrid, ValidFun, ClosedGrids, {XDirection, YDirection} = Direction, NeighbourGrids) ->
    case {X + XDirection, Y + YDirection} of
        EndGrid ->
            [Grid | NeighbourGrids];
        NextGrid ->
            case is_not_close(NextGrid, ClosedGrids) andalso ValidFun(NextGrid) of
                true ->
                    get_neighbour_girds_2(NextGrid, EndGrid, ValidFun, ClosedGrids, Direction, NeighbourGrids);
                false ->
                    NeighbourGrids
            end
    end.

get_neighbour_girds_2({X, Y} = Grid, EndGrid, ValidFun, CloseGrids, {_XDirection, 0} = Direction, NeighbourGrids) ->
    case not ValidFun({X, Y + 1}) orelse not ValidFun({X, Y - 1}) of
        true ->
            [Grid | NeighbourGrids];
        false ->
            get_neighbour_girds_1(Grid, EndGrid, ValidFun, CloseGrids, Direction, NeighbourGrids)
    end;
get_neighbour_girds_2({X, Y} = Grid, EndGrid, ValidFun, CloseGrids, {0, _YDirection} = Direction, NeighbourGrids) ->
    case not ValidFun({X + 1, Y}) orelse not ValidFun({X - 1, Y}) of
        true ->
            [Grid | NeighbourGrids];
        false ->
            get_neighbour_girds_1(Grid, EndGrid, ValidFun, CloseGrids, Direction, NeighbourGrids)
    end;
get_neighbour_girds_2({X, Y} = Grid, EndGrid, ValidFun, CloseGrids, {XDirection, YDirection} = Direction, NeighbourGrids) ->
    IsBlock1 = ValidFun({X - XDirection, Y}),
    IsBlock2 = ValidFun({X, Y - YDirection}),
    case IsBlock1 orelse IsBlock2 of
        true ->
            case (not IsBlock1 andalso ValidFun({X - XDirection, Y + YDirection}))
                orelse (not IsBlock2 andalso ValidFun({X + XDirection, Y - YDirection})) of
                true ->
                    [Grid | NeighbourGrids];
                false ->
                    case get_neighbour_girds_1(Grid, EndGrid, ValidFun, CloseGrids, {XDirection, 0}, []) of
                        [] ->
                            case get_neighbour_girds_1(Grid, EndGrid, ValidFun, CloseGrids, {0, YDirection}, []) of
                                [] ->
                                    get_neighbour_girds_1(Grid, EndGrid, ValidFun, CloseGrids, Direction, []);
                                _ ->
                                    [Grid | NeighbourGrids]
                            end;
                        _ ->
                            [Grid | NeighbourGrids]
                    end
            end;
        false ->
            NeighbourGrids
    end.

direction(Grid, Grid, _ValidFun) ->
    [{1, 0}, {1, 1}, {0, 1}, {-1, 1}, {-1, 0}, {-1, -1}, {0, -1}, {1, -1}];
direction({X1, Y1}, {X1, Y2}, ValidFun) ->
    YDirection = ?IF(Y1 > Y2, 1, -1),
    Directions1 = ?IF(ValidFun({X1 + 1, Y1}), [], [{1, YDirection}]),
    Directions2 = ?IF(ValidFun(X1 - 1, Y1), Directions1, [{-1, YDirection} | Directions1]),
    [{0, YDirection} | Directions2];
direction({X1, Y1}, {X2, Y1}, ValidFun) ->
    XDirection = ?IF(X1 > X2, 1, -1),
    Directions1 = ?IF(ValidFun({X1, Y1 + 1}), [], [{XDirection, 1}]),
    Directions2 = ?IF(ValidFun(X1, Y1 - 1), Directions1, [{XDirection, -1} | Directions1]),
    [{XDirection, 0} | Directions2];
direction({X1, Y1}, {X2, Y2}, ValidFun) ->
    XDirection = ?IF(X1 > X2, 1, -1),
    YDirection = ?IF(Y1 > Y2, 1, -1),
    Directions1 = ?IF(ValidFun({X1 - XDirection, Y1}), [], [{-XDirection, YDirection}]),
    Directions2 = ?IF(ValidFun({X1, Y1 - YDirection}), [], [{XDirection, -YDirection} | Directions1]),
    [{XDirection, YDirection}, {XDirection, 0}, {YDirection, 0} | Directions2].

is_not_close(Grid, ClosedGrids) ->
    case ClosedGrids of
        #{Grid => _} ->
            false;
        _ ->
            true
    end.

add_neighbour_girds(EndGrid, ParentGrid, G, OpenGrids, ParentGrids, [Grid | T]) ->
    G1 = G + g(Grid, EndGrid),
    Score = G1 + h(Grid, EndGrid),
    OpenGrids1 = add_open_grid(Score, Grid, G1, OpenGrids),
    ParentGrids1 = ParentGrids#{Grid => ParentGrid},
    add_neighbour_girds(EndGrid, ParentGrid, G, OpenGrids1, ParentGrids1, T);
add_neighbour_girds(_EndGrid, _ParentGrid, _G, OpenGrids, ParentGrids, []) ->
    {OpenGrids, ParentGrids}.

g({X1, Y1}, {X2, Y2}) ->
    X3 = erlang:abs(X1 - X2),
    Y3 = erlang:abs(Y1 - Y2),
    case X3 > Y3 of
        true ->
            14 * Y3 + 10 * (X3 - Y3);
        false ->
            14 * X3 + 10 * (Y3 - X3)
    end.

h({X1, Y1}, {X2, Y2}) ->
%%    X3 = X2 - X1,
%%    Y3 = Y2 - Y1,
%%    trunc(math:sqrt(X3 * X3 + Y3 * Y3) * 10).
    (erlang:abs(X1 - X2) + erlang:abs(Y1 - Y2)) * 10.


add_open_grid(Score, Grid, G, OpenGrids) ->
    gb_trees:insert({Score, Grid}, G, OpenGrids).

get_full_path(Grid, ParentGrids) ->
    case ParentGrids of
        #{Grid := ParentGrid} ->
            [ParentGrid | get_full_path(ParentGrid, ParentGrids)];
        _ ->
            []
    end.
