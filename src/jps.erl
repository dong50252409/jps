-module(jps).

-compile(inline).

%% API
-export([search/4, get_full_path/1]).

-type grid() :: {X :: non_neg_integer(), Y :: non_neg_integer()}.
-type result() :: {jump_points, [grid()]}|none|max_limited.
-type max_limit() :: {max_limit, non_neg_integer()}.
-type option() :: max_limit().
-type options() :: [option()].

-define(DEFAULT_MAX_LIMIT, 16#FFFF).

-define(IF(Condition, T, F), (
    case Condition of
        true ->
            T;
        false ->
            F
    end
)).


-spec search(StartGrid :: grid(), EndGrid :: grid(), ValidFun :: fun((grid()) -> boolean()), Options :: options()) -> result().
search(StartGrid, EndGrid, ValidFun, Options) ->
    OpenGrids = gb_trees:empty(),
    ClosedGrids = #{},
    Directions = [{1, 0}, {1, 1}, {0, 1}, {-1, 1}, {-1, 0}, {-1, -1}, {0, -1}, {1, -1}],
    JumpGrids = get_jump_grids(EndGrid, ValidFun, StartGrid, ClosedGrids, Directions),
    {OpenGrids1, ClosedGrids1} = add_jump_grids(EndGrid, StartGrid, 0, [StartGrid], OpenGrids, ClosedGrids, JumpGrids),
    draw_map(OpenGrids1),
    MaxLimit = proplists:get_value(max_limit, Options, ?DEFAULT_MAX_LIMIT),
    do_search(EndGrid, ValidFun, OpenGrids1, ClosedGrids1, MaxLimit).


-spec get_full_path(JumpPoints :: [grid()]) -> {full_path, Path :: [grid()]}.
get_full_path(JumpPoints) ->
    Path = get_full_path_1(JumpPoints, []),
    {full_path, lists:reverse(Path)}.

%%%======================================================================
%%% Internal Functions
%%%======================================================================
do_search(EndGrid, ValidFun, OpenGrids, ClosedGrids, MaxLimit) when MaxLimit > 0 ->
    case take_grid(OpenGrids) of
        none ->
            none;
        {EndGrid, _G, Path, _OpenGrids1} ->
            {jump_points, [EndGrid | Path]};
        {Grid, G, Path, OpenGrids1} ->
            ParentGrid = hd(Path),
            Directions = get_directions(ValidFun, Grid, ParentGrid),
            io:format("Directions:~w~n", [Directions]),
            io:format("take_grid Grid:~w ParentGrid:~w G:~w~n", [Grid, ParentGrid, G]),
            JumpGrids = get_jump_grids(EndGrid, ValidFun, Grid, ClosedGrids, Directions),
            {OpenGrids2, ClosedGrids1} = add_jump_grids(EndGrid, Grid, G, [Grid | Path], OpenGrids1, ClosedGrids, JumpGrids),
            draw_map(OpenGrids2),
            do_search(EndGrid, ValidFun, OpenGrids2, ClosedGrids1, MaxLimit - 1)
    end;
do_search(_EndGrid, _ValidFun, _OpenGrids, _ClosedGrids, _MaxLimit) ->
    max_limited.

take_grid(OpenGrids) ->
    case gb_trees:is_empty(OpenGrids) of
        true ->
            none;
        false ->
            {{_Score, Grid}, {G, Path}, OpenGrids1} = gb_trees:take_smallest(OpenGrids),
            {Grid, G, Path, OpenGrids1}
    end.

get_directions(ValidFun, Grid, ParentGrid) ->
    {DX, DY} = get_direction(Grid, ParentGrid),
    directions_1(ValidFun, Grid, DX, DY).

directions_1(ValidFun, {X, Y}, DX, 0) ->
    case ValidFun({X + DX, Y}) of
        true ->
            NeighbourGirds = [{DX, 0}],
            NeighbourGirds1 = ?IF(ValidFun({X, Y + 1}), NeighbourGirds, [{DX, 1} | NeighbourGirds]),
            ?IF(ValidFun({X, Y - 1}), NeighbourGirds1, [{DX, -1} | NeighbourGirds1]);
        false ->
            []
    end;
directions_1(ValidFun, {X, Y}, 0, DY) ->
    case ValidFun({X, Y + DY}) of
        true ->
            NeighbourGirds = [{0, DY}],
            NeighbourGirds1 = ?IF(ValidFun({X + 1, Y}), NeighbourGirds, [{1, DY} | NeighbourGirds]),
            ?IF(ValidFun({X - 1, Y}), NeighbourGirds1, [{-1, DY} | NeighbourGirds1]);
        false ->
            []
    end;
directions_1(ValidFun, {X, Y}, DX, DY) ->
    NeighbourGirds1 = ?IF(ValidFun({X, Y + DY}), [{0, DY}], []),
    case ?IF(ValidFun({X + DX, Y}), [{DX, 0} | NeighbourGirds1], NeighbourGirds1) of
        [] ->
            [];
        NeighbourGirds2 ->
            NeighbourGirds3 = ?IF(ValidFun({X + DX, Y + DY}), [{DX, DY} | NeighbourGirds2], NeighbourGirds2),
            NeighbourGirds4 = ?IF(ValidFun({X - DX, Y}), NeighbourGirds3, [{-DX, DY} | NeighbourGirds3]),
            ?IF(ValidFun({X, Y - DY}), NeighbourGirds4, [{DX, -DY} | NeighbourGirds4])
    end.

get_direction({X1, Y1}, {X2, Y2}) ->
    {get_direction_1(X1, X2), get_direction_1(Y1, Y2)}.

get_direction_1(P1, P2) when P1 > P2 ->
    1;
get_direction_1(P1, P2) when P1 < P2 ->
    -1;
get_direction_1(P1, P1) ->
    0.

get_jump_grids(EndGrid, ValidFun, CurGrid, ClosedGrids, [{DX, DY} | T]) ->
    case get_jump_gird(EndGrid, ValidFun, ClosedGrids, CurGrid, DX, DY) of
        none ->
            get_jump_grids(EndGrid, ValidFun, CurGrid, ClosedGrids, T);
        NeighbourGrid ->
            [NeighbourGrid | get_jump_grids(EndGrid, ValidFun, CurGrid, ClosedGrids, T)]
    end;
get_jump_grids(_EndGrid, _ValidFun, _CurGrid, _ClosedGrids, []) ->
    [].

get_jump_gird(EndGrid, ValidFun, ClosedGrids, {X, Y}, DX, DY) when DX =:= 0; DY =:= 0 ->
    case {X + DX, Y + DY} of
        EndGrid ->
            ?IF(ValidFun(EndGrid), EndGrid, none);
        NeighbourGrid ->
            case not maps:is_key(NeighbourGrid, ClosedGrids) andalso ValidFun(NeighbourGrid) of
                true ->
                    get_jump_grid_1(EndGrid, ValidFun, ClosedGrids, NeighbourGrid, DX, DY);
                false ->
                    none
            end
    end;
get_jump_gird(EndGrid, ValidFun, ClosedGrids, {X, Y}, DX, DY) ->
    case {X + DX, Y + DY} of
        EndGrid ->
            ?IF(ValidFun(EndGrid), EndGrid, none);
        NeighbourGrid ->
            case not maps:is_key(NeighbourGrid, ClosedGrids) andalso ValidFun(NeighbourGrid)
                andalso ValidFun({X + DX, Y}) orelse ValidFun({X, Y + DY}) of
                true ->
                    get_jump_grid_1(EndGrid, ValidFun, ClosedGrids, NeighbourGrid, DX, DY);
                false ->
                    none
            end
    end.

get_jump_grid_1(EndGrid, ValidFun, ClosedGrids, NeighbourGrid, DX, DY) when DX =:= 0; DY =:= 0 ->
    case check_jump_grid(ValidFun, NeighbourGrid, DX, DY) of
        true ->
            NeighbourGrid;
        false ->
            get_jump_gird(EndGrid, ValidFun, ClosedGrids, NeighbourGrid, DX, DY)
    end;
get_jump_grid_1(EndGrid, ValidFun, ClosedGrids, NeighbourGrid, DX, DY) ->
    case check_jump_grid(ValidFun, NeighbourGrid, DX, DY) of
        true ->
            NeighbourGrid;
        false ->
            case get_jump_gird(EndGrid, ValidFun, ClosedGrids, NeighbourGrid, DX, 0) =/= none
                orelse get_jump_gird(EndGrid, ValidFun, ClosedGrids, NeighbourGrid, 0, DY) =/= none of
                true ->
                    NeighbourGrid;
                false ->
                    get_jump_gird(EndGrid, ValidFun, ClosedGrids, NeighbourGrid, DX, DY)
            end
    end.

check_jump_grid(ValidFun, {X, Y}, DX, 0) ->
    ValidFun({X + DX, Y})
        andalso (ValidFun({X + DX, Y + 1}) andalso not ValidFun({X, Y + 1})
        orelse ValidFun({X + DX, Y - 1}) andalso not ValidFun({X, Y - 1}));
check_jump_grid(ValidFun, {X, Y}, 0, DY) ->
    ValidFun({X, Y + DY})
        andalso (ValidFun({X + 1, Y + DY}) andalso not ValidFun({X + 1, Y})
        orelse ValidFun({X - 1, Y + DY}) andalso not ValidFun({X - 1, Y}));
check_jump_grid(ValidFun, {X, Y}, DX, DY) ->
    ValidFun({X, Y + DY}) andalso ValidFun({X - DX, Y + DY}) andalso not ValidFun({X - DX, Y})
        orelse ValidFun({X + DX, Y}) andalso ValidFun({X + DX, Y - DY}) andalso not ValidFun({X, Y - DY}).

add_jump_grids(EndGrid, ParentGrid, G, Path, OpenGrids, ClosedGrids, [Grid | T]) ->
    G1 = G + g(Grid, ParentGrid),
    Score = G1 + h(Grid, EndGrid),
    io:format("add_grids Score:~w,Grid:~w,G1:~w~n", [Score, Grid, G1]),
    OpenGrids1 = gb_trees:insert({Score, Grid}, {G1, Path}, OpenGrids),
    ClosedGrids1 = ClosedGrids#{Grid => true},
    add_jump_grids(EndGrid, ParentGrid, G, Path, OpenGrids1, ClosedGrids1, T);
add_jump_grids(_EndGrid, _ParentGrid, _G, _Path, OpenGrids, ClosedGrids, []) ->
    {OpenGrids, ClosedGrids}.

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

get_full_path_1([Grid1, Grid2 | T], Path) ->
    {DX, DY} = get_direction(Grid2, Grid1),
    Path1 = get_full_path_2(Grid1, Grid2, DX, DY, Path),
    get_full_path_1([Grid2 | T], Path1);
get_full_path_1([_], Path) ->
    Path.

get_full_path_2({X, Y}, Grid2, DX, DY, Path) ->
    case {X + DX, Y + DY} of
        Grid2 ->
            [Grid2 | Path];
        Grid ->
            get_full_path_2(Grid, Grid2, DX, DY, [Grid | Path])
    end.
%%============================================================
%% TEST
%%============================================================
-compile(export_all).
test(Start, End, Map) ->
    put(map, Map),
    ValidFun =
        fun({X, Y}) ->
            X =< 10 andalso Y =< 10 andalso 0 < X andalso 0 < Y
                andalso element(X, element(Y, Map)) =:= 32
        end,
    search(Start, End, ValidFun, []).

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