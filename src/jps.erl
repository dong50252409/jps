-module(jps).

-compile(export_all).

-export([search/3, test/0, direction/2]).


search(StartGrid, EndGrid, ValidFun) ->
    Score = f(StartGrid, EndGrid),
    OpenGrids = add_grid(Score, StartGrid, [], gb_trees:empty()),
    CloseGrids = #{},
    do_search(EndGrid, ValidFun, OpenGrids, CloseGrids).

do_search(EndGrid, ValidFun, OpenGrids, CloseGrids) ->
    case gb_trees:is_empty(OpenGrids) of
        true ->
            none;
        false ->
            case gb_trees:take_smallest(OpenGrids) of
                {{_, EndGrid}, Path, _OpenGrids1} ->
                    [EndGrid | Path];
                {{_Score, Grid}, Path, OpenGrids1} ->
                    CloseGrids1 = CloseGrids#{Grid => ture},
                    Directions = direction(Grid, Path),
                    NeighbourGrids = search_neighbour_girds(EndGrid, ValidFun, Grid, CloseGrids1, Directions, []),
                    OpenGrids2 = add_neighbour_girds(EndGrid, NeighbourGrids, [Grid | Path], OpenGrids1),
%%                    draw_map(OpenGrids2),
                    do_search(EndGrid, ValidFun, OpenGrids2, CloseGrids1)
            end
    end.

direction(_, []) ->
    [{1, 0}, {1, 1}, {0, 1}, {-1, 1}, {-1, 0}, {-1, -1}, {0, -1}, {1, -1}];
direction({X1, Y1}, [{X1, Y2} | _]) ->
    case Y1 > Y2 of
        true ->
            [{0, 1}, {1, 1}, {1, -1}];
        false ->
            [{0, -1}, {-1, 1}, {-1, -1}]
    end;
direction({X1, Y1}, [{X2, Y1} | _]) ->
    case X1 > X2 of
        true ->
            [{1, 0}, {1, 1}, {1, -1}];
        false ->
            [{-1, 0}, {-1, 1}, {-1, -1}]
    end;
direction({X1, Y1}, [{X2, Y2} | _]) when X1 > X2 ->
    case Y1 > Y2 of
        true ->
            [{1, 1}];
        false ->
            [{1, -1}]
    end;
direction({_X1, Y1}, [{_X2, Y2} | _]) ->
    case Y1 > Y2 of
        true ->
            [{-1, 1}];
        false ->
            [{-1, -1}]
    end.

search_neighbour_girds(EndGrid, ValidFun, ParentGrid, CloseGrids, [Direction | T], NeighbourGrids) ->
    NeighbourGrids1 = search_neighbour_grids_1(EndGrid, ValidFun, ParentGrid, CloseGrids, Direction, NeighbourGrids),
    search_neighbour_girds(EndGrid, ValidFun, ParentGrid, CloseGrids, T, NeighbourGrids1);
search_neighbour_girds(_EndGrid, _ValidFun, _ParentGrid, _CloseGrids, [], NeighbourGrids) ->
    NeighbourGrids.

search_neighbour_grids_1(EndGrid, ValidFun, {X, Y}, CloseGrids, {XOffset, YOffset} = Direction, NeighbourGrids) ->
    case {X + XOffset, Y + YOffset} of
        EndGrid ->
            [EndGrid | NeighbourGrids];
        NextGrid ->
            case not maps:is_key(NextGrid, CloseGrids) andalso ValidFun(NextGrid) of
                true ->
                    search_neighbour_grids_2(EndGrid, ValidFun, NextGrid, CloseGrids, Direction, NeighbourGrids);
                false ->
                    NeighbourGrids
            end
    end.

search_neighbour_grids_2(EndGrid, ValidFun, Grid, CloseGrids, Direction, NeighbourGrids)
    when element(1, Direction) =:= 0; element(2, Direction) =:= 0 ->
    case check_neighbour_grid(Grid, ValidFun, Direction) of
        true ->
            [Grid | NeighbourGrids];
        false ->
            search_neighbour_grids_1(EndGrid, ValidFun, Grid, CloseGrids, Direction, NeighbourGrids)
    end;

search_neighbour_grids_2(EndGrid, ValidFun, Grid, CloseGrids, {XOffset, YOffset} = Direction, NeighbourGrids) ->
    case check_neighbour_grid(Grid, ValidFun, Direction) of
        true ->
            [Grid | NeighbourGrids];
        false ->
            case search_neighbour_grids_1(EndGrid, ValidFun, Grid, CloseGrids, {XOffset, 0}, []) of
                [] ->
                    case search_neighbour_grids_1(EndGrid, ValidFun, Grid, CloseGrids, {0, YOffset}, []) of
                        [] ->
                            search_neighbour_grids_1(EndGrid, ValidFun, Grid, CloseGrids, Direction, NeighbourGrids);
                        _ ->
                            [Grid | NeighbourGrids]
                    end;
                _ ->
                    [Grid | NeighbourGrids]
            end
    end.

check_neighbour_grid({X, Y}, ValidFun, {Offset, 0}) ->
    X1 = X + Offset,
    (not ValidFun({X, Y + 1}) andalso ValidFun({X1, Y + 1}))
        orelse (not ValidFun({X, Y - 1}) andalso ValidFun({X1, Y - 1}));
check_neighbour_grid({X, Y}, ValidFun, {0, Offset}) ->
    Y1 = Y + Offset,
    (not ValidFun({X + 1, Y}) andalso ValidFun({X + 1, Y1}))
        orelse (not ValidFun({X - 1, Y}) andalso ValidFun({X - 1, Y1}));
check_neighbour_grid({X, Y}, ValidFun, {XOffset, YOffset}) ->
    X1 = X - XOffset,
    Y1 = Y - YOffset,
    (not ValidFun({X, Y1}) andalso ValidFun({X + XOffset, Y1}))
        orelse (not ValidFun({X1, Y}) andalso ValidFun({X1, Y + YOffset})).

add_neighbour_girds(EndGrid, [Grid | T], Path, OpenGrids) ->
    Score = f(Grid, EndGrid),
    OpenGrids1 = add_grid(Score, Grid, Path, OpenGrids),
    add_neighbour_girds(EndGrid, T, Path, OpenGrids1);
add_neighbour_girds(_EndGrid, [], _Path, OpenGrids) ->
    OpenGrids.

f(Grid1, Grid2) ->
    g(Grid1, Grid2) + h(Grid1, Grid2).

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

add_grid(Score, Grid, Path, OpenGrids) ->
    gb_trees:insert({Score, Grid}, Path, OpenGrids).
%%============================================================
%% TEST
%%============================================================
test() ->
    Map = {
        % 1   2   3   4   5   6   7   8   9  10
        {$ , $ , $ , $ , $ , $ , $ , $ , $ , $ },    % 1
        {$ , $ , $ , $ , $X, $ , $ , $ , $ , $ },    % 2
        {$ , $ , $ , $ , $ , $ , $ , $ , $ , $ },    % 3
        {$ , $X, $ , $ , $ , $ , $ , $X, $ , $ },    % 4
        {$ , $ , $ , $ , $S, $ , $ , $ , $ , $ },    % 5
        {$ , $X, $ , $ , $ , $ , $ , $X, $ , $ },    % 6
        {$ , $ , $ , $ , $ , $ , $ , $ , $ , $ },    % 7
        {$ , $ , $ , $ , $X, $ , $ , $ , $ , $ },    % 8
        {$ , $ , $ , $ , $ , $ , $ , $ , $ , $ },    % 9
        {$ , $ , $ , $ , $ , $ , $ , $ , $ , $E}     % 10
    },
    put(map, Map),
    ValidFun =
        fun({X, Y}) ->
            X =< 10 andalso Y =< 10 andalso 0 < X andalso 0 < Y
                andalso element(X, element(Y, Map)) =:= 32
        end,
    search({5, 5}, {10, 10}, ValidFun).

take(OpenGrids, Map) ->
    case gb_trees:is_empty(OpenGrids) of
        true ->
            Map;
        false ->
            {{_Score, {X, Y}}, _Parent, OpenGrids1} = gb_trees:take_smallest(OpenGrids),
            take(OpenGrids1, setelement(Y, Map, setelement(X, element(Y, Map), $P)))
    end.


draw_map(OpenGrids) ->
    Map1 = take(OpenGrids, get(map)),
    put(map, Map1),
    io:format("   12345678910~n"),
    io:format("  ------------~n"),
    Fun =
        fun(T, N) ->
            io:format("~2w|~s|~n", [N, tuple_to_list(T)]),
            N + 1
        end,
    _ = lists:foldl(Fun, 1, tuple_to_list(Map1)),
    io:format("  ------------~n").