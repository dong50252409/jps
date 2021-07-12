-module(jps).

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
    G = 0,
    Score = G + h(StartGrid, EndGrid),
    OpenGrids = gb_trees:insert({Score, StartGrid}, {G, []}, gb_trees:empty()),
    VisitedGrids = #{StartGrid => G},
    MaxLimit = proplists:get_value(max_limit, Options, ?DEFAULT_MAX_LIMIT),
    do_search(EndGrid, ValidFun, OpenGrids, VisitedGrids, MaxLimit).

-spec get_full_path(JumpPoints :: [grid()]) -> {full_path, Path :: [grid()]}.
get_full_path(JumpPoints) ->
    Path = get_full_path_1(JumpPoints, []),
    {full_path, lists:reverse(Path)}.

%%%======================================================================
%%% Internal Functions
%%%======================================================================
do_search(EndGrid, ValidFun, OpenGrids, VisitedGrids, MaxLimit) when MaxLimit > 0 ->
%%    draw_map(OpenGrids),
    case gb_trees:is_empty(OpenGrids) of
        true ->
            none;
        false ->
            case gb_trees:take_smallest(OpenGrids) of
                {{_Score, EndGrid}, {_G, Path}, _OpenGrids1} ->
                    {jump_points, lists:reverse([EndGrid | Path])};
                {{_Score, Grid}, {G, Path}, OpenGrids1} ->
                    Directions = get_directions(ValidFun, Grid, Path),
%%                    io:format("take_grid Grid:~w ParentGrid:~w G:~w~n Directions:~w~n", [Grid, ParentGrid, G, Directions]),
                    VisitedGrids1 = VisitedGrids#{Grid := -1},
                    JumpGrids = get_jump_grids(EndGrid, ValidFun, Grid, VisitedGrids1, Directions),
                    {OpenGrids2, VisitedGrids2} = add_jump_grids(EndGrid, Grid, G, [Grid | Path], OpenGrids1, VisitedGrids1, JumpGrids),
                    do_search(EndGrid, ValidFun, OpenGrids2, VisitedGrids2, MaxLimit - 1)
            end
    end;
do_search(_EndGrid, _ValidFun, _OpenGrids, _VisitedGrids, _MaxLimit) ->
    max_limited.

get_directions(ValidFun, Grid, [ParentGrid | _]) ->
    {DX, DY} = get_direction(Grid, ParentGrid),
    directions_1(ValidFun, Grid, DX, DY);
get_directions(_ValidFun, _Grid, []) ->
    [{1, 0}, {1, 1}, {0, 1}, {-1, 1}, {-1, 0}, {-1, -1}, {0, -1}, {1, -1}].

directions_1(ValidFun, {X, Y}, DX, 0) ->
    NeighbourGirds = [{DX, 0}],
    NeighbourGirds1 = ?IF(ValidFun({X, Y + 1}), NeighbourGirds, [{DX, 1} | NeighbourGirds]),
    ?IF(ValidFun({X, Y - 1}), NeighbourGirds1, [{DX, -1} | NeighbourGirds1]);
directions_1(ValidFun, {X, Y}, 0, DY) ->
    NeighbourGirds = [{0, DY}],
    NeighbourGirds1 = ?IF(ValidFun({X + 1, Y}), NeighbourGirds, [{1, DY} | NeighbourGirds]),
    ?IF(ValidFun({X - 1, Y}), NeighbourGirds1, [{-1, DY} | NeighbourGirds1]);
directions_1(ValidFun, {X, Y}, DX, DY) ->
    NeighbourGirds1 = ?IF(ValidFun({X, Y + DY}), [{0, DY}], []),
    NeighbourGirds2 = ?IF(ValidFun({X + DX, Y}), [{DX, 0} | NeighbourGirds1], NeighbourGirds1),
    NeighbourGirds3 = ?IF(ValidFun({X + DX, Y + DY}), [{DX, DY} | NeighbourGirds2], NeighbourGirds2),
    NeighbourGirds4 = ?IF(ValidFun({X - DX, Y}), NeighbourGirds3, [{-DX, DY} | NeighbourGirds3]),
    ?IF(ValidFun({X, Y - DY}), NeighbourGirds4, [{DX, -DY} | NeighbourGirds4]).

get_direction({X1, Y1}, {X2, Y2}) ->
    {get_direction_1(X1, X2), get_direction_1(Y1, Y2)}.

get_direction_1(P1, P2) when P1 > P2 ->
    1;
get_direction_1(P1, P2) when P1 < P2 ->
    -1;
get_direction_1(P1, P1) ->
    0.

get_jump_grids(EndGrid, ValidFun, CurGrid, VisitedGrids, [{DX, DY} | T]) ->
    case get_jump_gird(EndGrid, ValidFun, VisitedGrids, CurGrid, DX, DY) of
        none ->
            get_jump_grids(EndGrid, ValidFun, CurGrid, VisitedGrids, T);
        NeighbourGrid ->
            [NeighbourGrid | get_jump_grids(EndGrid, ValidFun, CurGrid, VisitedGrids, T)]
    end;
get_jump_grids(_EndGrid, _ValidFun, _CurGrid, _VisitedGrids, []) ->
    [].

get_jump_gird(EndGrid, ValidFun, VisitedGrids, {X, Y}, DX, DY) ->
    case {X + DX, Y + DY} of
        EndGrid ->
            ?IF(ValidFun(EndGrid), EndGrid, none);
        {X1, Y1} = NeighbourGrid ->
            case maps:get(NeighbourGrid, VisitedGrids, 0) > -1 andalso ValidFun(NeighbourGrid) of
                true when DX =/= 0, DY =/= 0 ->
                    ?IF(ValidFun({X1, Y}) orelse ValidFun({X, Y1}),
                        get_jump_grid_1(EndGrid, ValidFun, VisitedGrids, NeighbourGrid, DX, DY), none);
                true ->
                    ?IF(ValidFun({X1 + DX, Y1 + DY}),
                        get_jump_grid_1(EndGrid, ValidFun, VisitedGrids, NeighbourGrid, DX, DY), none);
                false ->
                    none
            end
    end.

get_jump_grid_1(EndGrid, ValidFun, VisitedGrids, NeighbourGrid, DX, DY) when DX =:= 0; DY =:= 0 ->
    case check_jump_grid(ValidFun, NeighbourGrid, DX, DY) of
        true ->
            NeighbourGrid;
        false ->
            get_jump_gird(EndGrid, ValidFun, VisitedGrids, NeighbourGrid, DX, DY)
    end;
get_jump_grid_1(EndGrid, ValidFun, VisitedGrids, NeighbourGrid, DX, DY) ->
    case check_jump_grid(ValidFun, NeighbourGrid, DX, DY)
        orelse get_jump_gird(EndGrid, ValidFun, VisitedGrids, NeighbourGrid, DX, 0) =/= none
        orelse get_jump_gird(EndGrid, ValidFun, VisitedGrids, NeighbourGrid, 0, DY) =/= none of
        true ->
            NeighbourGrid;
        false ->
            get_jump_gird(EndGrid, ValidFun, VisitedGrids, NeighbourGrid, DX, DY)
    end.

check_jump_grid(ValidFun, {X, Y}, DX, 0) ->
    ValidFun({X + DX, Y + 1}) andalso not ValidFun({X, Y + 1})
        orelse ValidFun({X + DX, Y - 1}) andalso not ValidFun({X, Y - 1});
check_jump_grid(ValidFun, {X, Y}, 0, DY) ->
    ValidFun({X + 1, Y + DY}) andalso not ValidFun({X + 1, Y})
        orelse ValidFun({X - 1, Y + DY}) andalso not ValidFun({X - 1, Y});
check_jump_grid(ValidFun, {X, Y}, DX, DY) ->
    (ValidFun({X, Y + DY}) andalso ValidFun({X - DX, Y + DY}) andalso not ValidFun({X - DX, Y}))
        orelse (ValidFun({X + DX, Y}) andalso ValidFun({X + DX, Y - DY}) andalso not ValidFun({X, Y - DY})).

add_jump_grids(EndGrid, ParentGrid, G, Path, OpenGrids, VisitedGrids, [Grid | T]) ->
    G1 = G + g(Grid, ParentGrid),
    Score = G1 + h(Grid, EndGrid),
    case VisitedGrids of
        #{Grid := OldScore} when OldScore =< Score ->
            OpenGrids2 = OpenGrids,
            VisitedGrids1 = VisitedGrids;
        #{Grid := OldScore} when OldScore > Score ->
%%            io:format("replace_add_grids OldScore:~w Score:~w Grid:~w G1:~w~n", [OldScore, Score, Grid, G1]),
            OpenGrids1 = gb_trees:delete({OldScore, Grid}, OpenGrids),
            OpenGrids2 = gb_trees:insert({Score, Grid}, {G1, Path}, OpenGrids1),
            VisitedGrids1 = VisitedGrids#{Grid => Score};
        _ ->
%%            io:format("add_grids Score:~w Grid:~w G1:~w~n", [Score, Grid, G1]),
            OpenGrids2 = gb_trees:insert({Score, Grid}, {G1, Path}, OpenGrids),
            VisitedGrids1 = VisitedGrids#{Grid => Score}
    end,
    add_jump_grids(EndGrid, ParentGrid, G, Path, OpenGrids2, VisitedGrids1, T);
add_jump_grids(_EndGrid, _ParentGrid, _G, _Path, OpenGrids, VisitedGrids, []) ->
    {OpenGrids, VisitedGrids}.

g({X1, Y1}, {X2, Y2}) ->
    X3 = erlang:abs(X1 - X2),
    Y3 = erlang:abs(Y1 - Y2),
    ?IF(X3 > Y3, 14 * Y3 + 10 * (X3 - Y3), 14 * X3 + 10 * (Y3 - X3)).

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
-ifdef(TEST).
-compile(export_all).

test(Row, Col, BlockNum, Num, IsShow) ->
    WorldMaps = [gen_map(Row, Col, BlockNum) || _ <- lists:seq(1, Num)],
    file:write_file("world_maps.data", term_to_binary(WorldMaps)),
    test_1(Row, Col, IsShow, WorldMaps).

test(Row, Col, IsShow) ->
    {ok, Bin} = file:read_file("world_maps.data"),
    WorldMaps = binary_to_term(Bin),
    test_1(Row, Col, IsShow, WorldMaps).

test_1(Row, Col, IsShow, WorldMaps) ->
    {ok, IO} = file:open("world_maps.out", [write]),
    put(io, IO),
    loop_world_map({1, 1}, {Row, Col}, Row, Col, WorldMaps, IsShow),
    file:close(IO).

loop_world_map(Start, End, Row, Col, [WorldMap | T], IsShow) ->
    put(world_map, WorldMap),
    Fun =
        fun({X, Y}) ->
            X > 0 andalso X =< Row andalso Y > 0 andalso Y =< Col
                andalso element(X, element(Y, WorldMap)) =/= $X
        end,
    case jps:search(Start, End, Fun, []) of
        {jump_points, Path} ->
            {full_path, FullPath} = jps:get_full_path(Path);
        _Other ->
            FullPath = []
    end,
    ?IF(IsShow, draw_map(Row, FullPath, WorldMap), ok),
    loop_world_map(Start, End, Row, Col, T, IsShow);
loop_world_map(_Start, _End, _Row, _Col, [], _IsShow) ->
    ok.

gen_map(Row, Col, BlockNum) ->
    WorldMap = list_to_tuple(lists:duplicate(Col, list_to_tuple(lists:duplicate(Row, $ )))),
    gen_block(Row, Col, BlockNum, WorldMap).

gen_block(Row, Col, BlockNum, WorldMap) when BlockNum > 0 ->
    case {rand:uniform(Row), rand:uniform(Col)} of
        XY when XY =:= {1, 1}; XY =:= {Row, 1}; XY =:= {1, Col}; XY =:= {Row, Col} ->
            gen_block(Row, Col, BlockNum, WorldMap);
        {X, Y} ->
            C = element(Y, WorldMap),
            case element(X, C) =/= $X of
                true ->
                    WorldMap1 = setelement(Y, WorldMap, setelement(X, C, $X)),
                    gen_block(Row, Col, BlockNum - 1, WorldMap1);
                false ->
                    gen_block(Row, Col, BlockNum, WorldMap)
            end
    end;
gen_block(_Row, _Col, _BlockNum, WorldMap) ->
    WorldMap.

draw_map(Width, Path, WorldMap) ->
    WorldMap1 = draw_map_path(Path, WorldMap),
    Border = io_lib:format("~s~n", [lists:duplicate(Width + 2, $X)]),
    Str = [io_lib:format("X~sX~n", [tuple_to_list(Row)]) || Row <- tuple_to_list(WorldMap1)],
    io:format(get(io), "~s~n", [[Border, Str, Border]]).

draw_map_path([{X, Y} | T], WorldMap) ->
    WorldMap1 = setelement(Y, WorldMap, setelement(X, element(Y, WorldMap), $o)),
    draw_map_path(T, WorldMap1);
draw_map_path(_, WorldMap) ->
    WorldMap.

draw_map(OpenGrids) ->
    Map1 = take(OpenGrids, get(world_map)),
    put(world_map, Map1),
    io:format("   12345678910~n"),
    io:format("  XXXXXXXXXXXX~n"),
    Fun =
        fun(T, N) ->
            io:format("~2wX~sX~n", [N, tuple_to_list(T)]),
            N + 1
        end,
    _ = lists:foldl(Fun, 1, tuple_to_list(Map1)),
    io:format("  XXXXXXXXXXXX~n").

take(OpenGrids, Map) ->
    case gb_trees:is_empty(OpenGrids) of
        true ->
            Map;
        false ->
            {{_Score, {X, Y}}, _Parent, OpenGrids1} = gb_trees:take_smallest(OpenGrids),
            take(OpenGrids1, setelement(Y, Map, setelement(X, element(Y, Map), $P)))
    end.
-endif.