-module(jps).

%% API
-export([search/3, get_full_path/2]).
-export([test/3]).

-define(IF(Condition, T, F), (
    case Condition of
        true ->
            T;
        false ->
            F
    end
)).

search(StartGrid, EndGrid, ValidFun) ->
    Score = h(StartGrid, EndGrid),
    OpenGrids = add_open_grid(Score, StartGrid, 0, gb_trees:empty()),
    ClosedGirds = #{},
    ParentGrids = #{},
    case do_search(EndGrid, ValidFun, OpenGrids, ClosedGirds, ParentGrids) of
        none ->
            none;
        {EndGrid, ParentGrids1} ->
            get_point_path(EndGrid, ParentGrids1, [])
    end.

get_full_path(StartGrid, PointList) ->
    get_full_path_1([StartGrid | PointList]).

%%%======================================================================
%%% Internal Functions
%%%======================================================================
do_search(EndGrid, ValidFun, OpenGrids, ClosedGirds, ParentGrids) ->
    case gb_trees:is_empty(OpenGrids) of
        true ->
            none;
        false ->
            case gb_trees:take_smallest(OpenGrids) of
                {{_, EndGrid}, _G, _OpenGrids1} ->
                    {EndGrid, ParentGrids};
                {{_Score, Grid}, G, OpenGrids1} ->
                    io:format("Score:~w,Grid:~w,G:~w~n", [_Score, Grid, G]),
                    ClosedGirds1 = ClosedGirds#{Grid => true},
                    ParentGrid = maps:get(Grid, ParentGrids, none),
                    Directions = direction(Grid, ParentGrid, ValidFun),
                    NeighbourGrids = get_neighbour_girds(Grid, EndGrid, ValidFun, ClosedGirds1, Directions, []),
                    {OpenGrids2, ParentGrids1} = add_neighbour_girds(EndGrid, Grid, G, OpenGrids1, ParentGrids, NeighbourGrids),
                    draw_map(OpenGrids2),
                    do_search(EndGrid, ValidFun, OpenGrids2, ClosedGirds1, ParentGrids1)
            end
    end.

get_neighbour_girds({X, Y} = Grid, EndGrid, ValidFun, ClosedGrids, [{XDirection, YDirection} = Direction | T], NeighbourGrids) ->
    case {X + XDirection, Y + YDirection} of
        EndGrid ->
            NeighbourGrids1 = [EndGrid | NeighbourGrids];
        NextGrid ->
            case is_not_close(NextGrid, ClosedGrids) andalso ValidFun(NextGrid) of
                true ->
                    NeighbourGrids1 = get_neighbour_girds_1(NextGrid, EndGrid, ValidFun, ClosedGrids, Direction, NeighbourGrids);
                false ->
                    NeighbourGrids1 = NeighbourGrids
            end
    end,
    get_neighbour_girds(Grid, EndGrid, ValidFun, ClosedGrids, T, NeighbourGrids1);
get_neighbour_girds(_Grid, _EndGrid, _ValidFun, _ClosedGrids, [], NeighbourGrids) ->
    NeighbourGrids.

get_neighbour_girds_1({X, Y} = Grid, EndGrid, ValidFun, ClosedGrids, {XDirection, 0} = Direction, NeighbourGrids) ->
    case ValidFun({X + XDirection, Y})
        andalso not ValidFun({X, Y + 1}) andalso ValidFun({X + XDirection, Y + 1})
        orelse not ValidFun({X, Y - 1}) andalso ValidFun({X + XDirection, Y - 1}) of
        true ->
            [Grid | NeighbourGrids];
        false ->
            get_neighbour_girds(Grid, EndGrid, ValidFun, ClosedGrids, [Direction], NeighbourGrids)
    end;

get_neighbour_girds_1({X, Y} = Grid, EndGrid, ValidFun, ClosedGrids, {0, YDirection} = Direction, NeighbourGrids) ->
    case ValidFun({X, Y + YDirection})
        andalso not ValidFun({X + 1, Y}) andalso ValidFun({X + 1, Y + YDirection})
        orelse not ValidFun({X - 1, Y}) andalso ValidFun({X - 1, Y + YDirection}) of
        true ->
            [Grid | NeighbourGrids];
        false ->
            get_neighbour_girds(Grid, EndGrid, ValidFun, ClosedGrids, [Direction], NeighbourGrids)
    end;
get_neighbour_girds_1({X, Y} = Grid, EndGrid, ValidFun, ClosedGrids, {XDirection, YDirection} = Direction, NeighbourGrids) ->
    IsNotBlockLeft = ValidFun({X - XDirection, Y}),
    IsNotBlockDown = ValidFun({X, Y - YDirection}),
    case IsNotBlockLeft orelse IsNotBlockDown of
        true ->
            case not IsNotBlockLeft andalso ValidFun({X - XDirection, Y + 1})
                orelse not IsNotBlockDown andalso ValidFun({X + 1, Y - YDirection}) of
                true ->
                    [Grid | NeighbourGrids];
                false ->
                    case get_neighbour_girds(Grid, EndGrid, ValidFun, ClosedGrids, [{XDirection, 0}], []) of
                        [] ->
                            case get_neighbour_girds(Grid, EndGrid, ValidFun, ClosedGrids, [{0, YDirection}], []) of
                                [] ->
                                    case get_neighbour_girds(Grid, EndGrid, ValidFun, ClosedGrids, [Direction], []) of
                                        [] ->
                                            NeighbourGrids;
                                        NeighbourGrids1 ->
                                            NeighbourGrids1 ++ NeighbourGrids
                                    end;
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

direction(_Grid, none, _ValidFun) ->
    [{1, 0}, {1, 1}, {0, 1}, {-1, 1}, {-1, 0}, {-1, -1}, {0, -1}, {1, -1}];
direction({X1, Y1}, {X1, Y2}, ValidFun) ->
    YDirection = ?IF(Y1 > Y2, 1, -1),
    Directions1 = ?IF(ValidFun({X1 + 1, Y1}), [], [{1, YDirection}]),
    Directions2 = ?IF(ValidFun({X1 - 1, Y1}), Directions1, [{-1, YDirection} | Directions1]),
    [{0, YDirection} | Directions2];
direction({X1, Y1}, {X2, Y1}, ValidFun) ->
    XDirection = ?IF(X1 > X2, 1, -1),
    Directions1 = ?IF(ValidFun({X1, Y1 + 1}), [], [{XDirection, 1}]),
    Directions2 = ?IF(ValidFun({X1, Y1 - 1}), Directions1, [{XDirection, -1} | Directions1]),
    [{XDirection, 0} | Directions2];
direction({X1, Y1}, {X2, Y2}, ValidFun) ->
    XDirection = ?IF(X1 > X2, 1, -1),
    YDirection = ?IF(Y1 > Y2, 1, -1),
    Directions1 = ?IF(ValidFun({X1 - XDirection, Y1}), [], [{-XDirection, YDirection}]),
    Directions2 = ?IF(ValidFun({X1, Y1 - YDirection}), Directions1, [{XDirection, -YDirection} | Directions1]),
    [{XDirection, YDirection}, {XDirection, 0}, {0, YDirection} | Directions2].

is_not_close(Grid, ClosedGrids) ->
    case ClosedGrids of
        #{Grid := _} ->
            false;
        _ ->
            true
    end.

add_neighbour_girds(EndGrid, ParentGrid, G, OpenGrids, ParentGrids, [Grid | T]) ->
    G1 = G + g(Grid, ParentGrid),
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
    (erlang:abs(X1 - X2) + erlang:abs(Y1 - Y2)) * 10.


add_open_grid(Score, Grid, G, OpenGrids) ->
    gb_trees:insert({Score, Grid}, G, OpenGrids).

get_point_path(Grid, ParentGrids, Path) ->
    case ParentGrids of
        #{Grid := ParentGrid} ->
            get_point_path(ParentGrid, ParentGrids, [Grid | Path]);
        #{} ->
            Path
    end.

get_full_path_1([Grid1, Grid2 | T]) ->
    {XD, YD} = get_direction(Grid1, Grid2),
    get_full_path_2(Grid1, Grid2, XD, YD) ++ get_full_path_1([Grid2 | T]);
get_full_path_1([_]) ->
    [].

get_full_path_2({X1, Y1}, ParentGrid, XD, YD) ->
    case {X1 + XD, Y1 + YD} of
        ParentGrid ->
            [ParentGrid];
        Grid ->
            [Grid | get_full_path_2(Grid, ParentGrid, XD, YD)]
    end.

get_direction({X1, Y1}, {X2, Y2}) ->
    if
        X1 > X2 ->
            XD = -1;
        X1 < X2 ->
            XD = 1;
        true ->
            XD = 0
    end,
    if
        Y1 > Y2 ->
            YD = -1;
        Y1 < Y2 ->
            YD = 1;
        true ->
            YD = 0
    end,
    {XD, YD}.

%%============================================================
%% TEST
%%============================================================
test(Start, End, Map) ->
    put(map, Map),
    ValidFun =
        fun({X, Y}) ->
            X =< 10 andalso Y =< 10 andalso 0 < X andalso 0 < Y
                andalso element(X, element(Y, Map)) =:= 32
        end,
    search(Start, End, ValidFun).

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
    io:format("  XXXXXXXXXXXX~n"),
    Fun =
        fun(T, N) ->
            io:format("~2wX~sX~n", [N, tuple_to_list(T)]),
            N + 1
        end,
    _ = lists:foldl(Fun, 1, tuple_to_list(Map1)),
    io:format("  XXXXXXXXXXXX~n").