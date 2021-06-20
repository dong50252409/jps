-module(jps).

-export([search/3, test/0, direction/2]).

search(StartGrid, EndGrid, ValidFun) ->
    Directions = [{1, 0}, {1, 1}, {0, 1}, {-1, 1}, {-1, 0}, {-1, -1}, {0, -1}, {1, -1}],
    OpenGrids = init_search(StartGrid, EndGrid, ValidFun, Directions, [], gb_trees:empty()),
    do_search(EndGrid, ValidFun, OpenGrids).

init_search(StartGrid, EndGrid, ValidFun, [Direction | T], NeighbourGrids, OpenGrids) ->
    NeighbourGrids1 = do_search_neighbour_grids(StartGrid, EndGrid, ValidFun, Direction, NeighbourGrids),
    init_search(StartGrid, EndGrid, ValidFun, T, NeighbourGrids1, OpenGrids);
init_search(StartGrid, EndGrid, _ValidFun, [], NeighbourGrids, OpenGrids) ->
    Fun =
        fun(Grid, AccOpenGrids) ->
            Score = f(Grid, EndGrid),
            add_grid(Score, Grid, [StartGrid], AccOpenGrids)
        end,
    lists:foldl(Fun, OpenGrids, NeighbourGrids).

do_search_neighbour_grids({X, Y}, EndGrid, ValidFun, {XOffset, YOffset} = Direction, NeighbourGrids) ->
    case {X + XOffset, Y + YOffset} of
        EndGrid ->
            [EndGrid | NeighbourGrids];
        NextGrid ->
            case ValidFun(NextGrid) of
                true ->
                    do_search_neighbour_grids_1(NextGrid, EndGrid, ValidFun, Direction, NeighbourGrids);
                false ->
                    NeighbourGrids
            end
    end.

do_search_neighbour_grids_1(Grid, EndGrid, ValidFun, Direction, NeighbourGrids)
    when element(1, Direction) =:= 0; element(2, Direction) =:= 0 ->
    case check_neighbour_grid(Grid, ValidFun, Direction) of
        true ->
            [Grid | NeighbourGrids];
        false ->
            do_search_neighbour_grids(Grid, EndGrid, ValidFun, Direction, NeighbourGrids)
    end;

do_search_neighbour_grids_1(Grid, EndGrid, ValidFun, {XOffset, YOffset} = Direction, NeighbourGrids) ->
    case check_neighbour_grid(Grid, ValidFun, Direction) of
        true ->
            [Grid | NeighbourGrids];
        false ->
            case do_search_neighbour_grids(Grid, EndGrid, ValidFun, {XOffset, 0}, []) of
                [] ->
                    case do_search_neighbour_grids(Grid, EndGrid, ValidFun, {0, YOffset}, []) of
                        [] ->
                            do_search_neighbour_grids(Grid, EndGrid, ValidFun, Direction, NeighbourGrids);
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

f(Grid1, Grid2) ->
    g(Grid1, Grid2) + h(Grid1, Grid2).

g({X, Y1}, {X, Y2}) ->
    10 * erlang:abs(Y1 - Y2);
g({X1, Y}, {X2, Y}) ->
    10 * erlang:abs(X1 - X2);
g({X1, _}, {X2, _}) ->
    14 * erlang:abs(X1 - X2).

h({X1, Y1}, {X2, Y2}) ->
    (erlang:abs(X1 - X2) + erlang:abs(Y1 - Y2)) * 10.

add_grid(Score, Grid, Path, OpenGrids) ->
    gb_trees:insert({Score, Grid}, Path, OpenGrids).

do_search(EndGrid, ValidFun, OpenGrids) ->
    case gb_trees:is_empty(OpenGrids) of
        true ->
            none;
        false ->
            case gb_trees:take_smallest(OpenGrids) of
                {{_, EndGrid}, Path, _OpenGrids1} ->
                    Path;
                {{_, Grid}, Path, OpenGrids1} ->
                    Directions = direction(Grid, hd(Path)),
                    OpenGrids2 = init_search(Grid, EndGrid, ValidFun, Directions, [], OpenGrids1),
                    draw_map(OpenGrids),
                    do_search(EndGrid, ValidFun, OpenGrids2)
            end
    end.


direction({X1, Y1}, {X1, Y2}) ->
    case Y1 > Y2 of
        true ->
            [{0, 1}, {1, 1}, {1, -1}];
        false ->
            [{0, -1}, {-1, 1}, {-1, -1}]
    end;
direction({X1, Y1}, {X2, Y1}) ->
    case X1 > X2 of
        true ->
            [{1, 0}, {1, 1}, {1, -1}];
        false ->
            [{-1, 0}, {-1, 1}, {-1, -1}]
    end;
direction({X1, Y1}, {X2, Y2}) when X1 > X2 ->
    case Y1 > Y2 of
        true ->
            [{1, 1}];
        false ->
            [{1, -1}]
    end;
direction({_X1, Y1}, {_X2, Y2}) ->
    case Y1 > Y2 of
        true ->
            [{-1, 1}];
        false ->
            [{-1, -1}]
    end.

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
    OpenGrids = search({5, 5}, {10, 10}, ValidFun),
    draw_map(OpenGrids).

take(OpenGrids, Map) ->
    case gb_trees:is_empty(OpenGrids) of
        true ->
            Map;
        false ->
            {{_Score, {X, Y}}, _Parent, OpenGrids1} = gb_trees:take_smallest(OpenGrids),
            io:format("Score:~w Grid:~w ParentGrid:~w~n", [_Score, {X, Y}, _Parent]),
            take(OpenGrids1, setelement(Y, Map, setelement(X, element(Y, Map), $P)))
    end.


draw_map(OpenGrids) ->
    Map1 = take(OpenGrids, get(map)),
    io:format("   12345678910~n"),
    io:format("  ------------~n"),
    Fun =
        fun(T, N) ->
            io:format("~2w|~s|~n", [N, tuple_to_list(T)]),
            N + 1
        end,
    _ = lists:foldl(Fun, 1, tuple_to_list(Map1)),
    io:format("  ------------~n").