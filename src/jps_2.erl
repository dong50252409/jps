%%%-------------------------------------------------------------------
%%% @author gz1417
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. 7月 2021 13:49
%%%-------------------------------------------------------------------
-module(jps_2).
-author("gz1417").

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
    OpenGrids = gb_trees:empty(),
    ClosedGrids = #{StartGrid => true},
    Directions = [{1, 0}, {1, 1}, {0, 1}, {-1, 1}, {-1, 0}, {-1, -1}, {0, -1}, {1, -1}],
    OpenGrids1 = get_jump_grids(EndGrid, ValidFun, StartGrid, 0, [StartGrid], OpenGrids, ClosedGrids, Directions),
    draw_map(OpenGrids1),
    do_search(EndGrid, ValidFun, OpenGrids1, ClosedGrids).

get_full_path(StartGrid, PointList) ->
    get_full_path_1([StartGrid | PointList]).

%%%======================================================================
%%% Internal Functions
%%%======================================================================
do_search(EndGrid, ValidFun, OpenGrids, ClosedGrids) ->
    case take_grid(OpenGrids, ClosedGrids) of
        none ->
            none;
        {EndGrid, _G, Path, _OpenGrids1, _CloseGrids1} ->
            lists:reverse([EndGrid | Path]);
        {Grid, G, Path, OpenGrids1, ClosedGrids1} ->
            NeighbourGrids = directions(ValidFun, Grid, Path),
            OpenGrids2 = get_jump_grids(EndGrid, ValidFun, Grid, G, [Grid | Path], OpenGrids1, ClosedGrids1, NeighbourGrids),
            draw_map(OpenGrids2),
            do_search(EndGrid, ValidFun, OpenGrids2, ClosedGrids1)
    end.

take_grid(OpenGrids, ClosedGrids) ->
    case gb_trees:is_empty(OpenGrids) of
        true ->
            none;
        false ->
            {{_Score, Grid}, {G, Path}, OpenGrids1} = gb_trees:take_smallest(OpenGrids),
            io:format("Score:~w,Grid:~w,G:~w~n", [_Score, Grid, G]),
            ClosedGrids1 = ClosedGrids#{Grid => true},
            {Grid, G, Path, OpenGrids1, ClosedGrids1}
    end.

directions(ValidFun, Grid, [ParentGrid | _]) ->
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
            NeighbourGirds = [{0, Y}],
            NeighbourGirds1 = ?IF(ValidFun({X + 1, Y}), NeighbourGirds, [{1, DY} | NeighbourGirds]),
            ?IF(ValidFun({X - 1, Y}), NeighbourGirds1, [{-1, DY} | NeighbourGirds1]);
        false ->
            []
    end;
directions_1(ValidFun, {X, Y}, DX, DY) ->
    NeighbourGirds1 = ?IF(ValidFun({X, Y + DY}), [{0, DY}], []),
    NeighbourGirds2 = ?IF(ValidFun({X + DX, Y}), [{DX, 0} | NeighbourGirds1], NeighbourGirds1),
    case NeighbourGirds2 of
        [] ->
            [];
        NeighbourGirds2 ->
            NeighbourGirds3 = ?IF(ValidFun({X + DX, Y + DY}), [{DX, DY} | NeighbourGirds2], NeighbourGirds2),
            NeighbourGirds4 = ?IF(ValidFun({X - DX, Y}), NeighbourGirds3, [{-DX, DY} | NeighbourGirds3]),
            ?IF(ValidFun({X, Y - DY}), NeighbourGirds4, [{DX, -DY} | NeighbourGirds4])
    end.

get_direction({X1, Y1}, {X2, Y2}) ->
    if
        X1 > X2 ->
            DX = 1;
        X1 < X2 ->
            DX = -1;
        true ->
            DX = 0
    end,
    if
        Y1 > Y2 ->
            DY = 1;
        Y1 < Y2 ->
            DY = -1;
        true ->
            DY = 0
    end,
    {DX, DY}.

get_jump_grids(EndGrid, ValidFun, CurGrid, G, Path, OpenGrids, ClosedGrids, [{DX, DY} | T]) ->
    case get_jump_gird(EndGrid, ValidFun, ClosedGrids, CurGrid, DX, DY) of
        none ->
            get_jump_grids(EndGrid, ValidFun, CurGrid, G, Path, OpenGrids, ClosedGrids, T);
        JumpPoint ->
            OpenGrids1 = add_jump_grids(JumpPoint, G, Path, OpenGrids),
            get_jump_grids(EndGrid, ValidFun, CurGrid, G, Path, OpenGrids1, ClosedGrids, T)
    end;
get_jump_grids(_EndGrid, _ValidFun, _CurGrid, _G, _Path, OpenGrids, _ClosedGrids, []) ->
    OpenGrids.

get_jump_gird(EndGrid, ValidFun, ClosedGrids, {X, Y}, DX, DY) ->
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
    end.

get_jump_grid_1(EndGrid, ValidFun, ClosedGrids, NeighbourGrid, DX, DY) when DX =:= 0; DY =:= 0 ->
    case check_jump_grid(ValidFun, NeighbourGrid, DX, DY) of
        true ->
            NeighbourGrid;
        false ->
            {X, Y} = NeighbourGrid,
            get_jump_gird(EndGrid, ValidFun, ClosedGrids, {X + DX, Y + DY}, DX, DY)
    end;
get_jump_grid_1(EndGrid, ValidFun, ClosedGrids, NeighbourGrid, DX, DY) ->
    case check_jump_grid(ValidFun, NeighbourGrid, DX, DY) of
        true ->
            NeighbourGrid;
        false ->
            {X, Y} = NeighbourGrid,
            case get_jump_gird(EndGrid, ValidFun, ClosedGrids, {X + DX, Y}, DX, DY) =/= none
                orelse get_jump_gird(EndGrid, ValidFun, ClosedGrids, {X, Y + DY}, DX, DY) =/= none of
                true ->
                    NeighbourGrid;
                false ->
                    get_jump_gird(EndGrid, ValidFun, ClosedGrids, {X + DX, Y + DY}, DX, DY)
            end
    end.

check_jump_grid(ValidFun, {X, Y}, DX, 0) ->
    ValidFun({X + DX, Y + 1}) andalso not ValidFun({X, Y + 1})
        orelse ValidFun({X + DX, Y - 1}) andalso not ValidFun({X, Y - 1});
check_jump_grid(ValidFun, {X, Y}, 0, DY) ->
    ValidFun({X + 1, Y + DY}) andalso not ValidFun({X + 1, Y})
        orelse ValidFun({X - 1, Y - DY}) andalso not ValidFun({X - 1, Y});
check_jump_grid(ValidFun, {X, Y}, DX, DY) ->
    ValidFun({X - DX, Y + DY}) andalso not ValidFun({X, Y + DY})
        orelse ValidFun({X + DX, Y - DY}) andalso not ValidFun({X + DX, Y}).

add_jump_grids(Grid, G, ParentPath, OpenGrids) ->
    ParentGrid = erlang:hd(ParentPath),
    G1 = G + g(Grid, ParentGrid),
    Score = G1 + h(Grid, ParentGrid),
    gb_trees:insert({Score, Grid}, {G1, ParentPath}, OpenGrids).

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

get_full_path_1([Grid1, Grid2 | T]) ->
    {DX, DY} = get_direction(Grid2, Grid1),
    get_full_path_2(Grid1, Grid2, DX, DY) ++ get_full_path_1([Grid2 | T]);
get_full_path_1([_]) ->
    [].

get_full_path_2({X1, Y1}, ParentGrid, XD, YD) ->
    case {X1 + XD, Y1 + YD} of
        ParentGrid ->
            [ParentGrid];
        Grid ->
            [Grid | get_full_path_2(Grid, ParentGrid, XD, YD)]
    end.


%%============================================================
%% TEST
%%============================================================
test(Start, End, Map) ->
%%    Map = {{32,88,32,32,88,32,32,32,32,32},{32,32,32,32,32,32,32,32,32,32},{32,32,32,32,32,32,32,32,32,32},{32,32,32,88,32,32,32,32,32,32},{32,32,32,32,32,32,32,88,32,32},{32,32,32,32,32,32,32,32,32,32},{88,32,32,32,88,32,32,32,32,32},{88,32,32,32,88,32,32,32,32,32},{32,32,32,32,32,32,32,32,32,88},{32,88,32,32,32,32,32,32,32,32}}
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