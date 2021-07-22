%%%-------------------------------------------------------------------
%%% @author gz1417
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 跳点寻路
%%% @end
%%% Created : 19. 7月 2021 11:47
%%%-------------------------------------------------------------------
-module(jps).

-include("jps.hrl").

%% API
-export([search/4, get_full_path/1]).

-export_type([grid/0, direction/0, valid_fun/0, visited_grids/0, result/0]).

-type grid() :: {X :: non_neg_integer(), Y :: non_neg_integer()}.
-type direction() :: {-1|0|1, -1|0|1}.
-type valid_fun() :: fun((grid()) -> boolean()).
-type visited_grids() :: #{JumpPointGrid :: grid() => -1 | non_neg_integer()}.
-type result() :: {jump_points, [grid()]}|none|max_limited.
-type max_limit() :: {max_limit, non_neg_integer()}.
-type option() :: max_limit() |{jps_mod, module()}.
-type options() :: [option()].

-callback(identity_successors(EndGrid :: grid(), ValidFun :: valid_fun(), VisitedGrids :: visited_grids(),
    CurGrid :: grid(), ParentPath :: [grid()]) -> JumpPoints :: [grid()]).
-callback(g(Grid1 :: grid(), Grid2 :: grid()) -> G :: number()).
-callback(h(Grid1 :: grid(), Grid2 :: grid()) -> H :: number()).
%%%======================================================================
%%% API Functions
%%%======================================================================
-spec search(StartGrid :: grid(), EndGrid :: grid(), ValidFun :: valid_fun(), Options :: options()) -> result().
search(StartGrid, EndGrid, ValidFun, Options) ->
    G = 0,
    OpenGrids = gb_trees:empty(),
    VisitedGrids = #{StartGrid => -1},
    JPSMod = proplists:get_value(jps_mod, Options, jps_eight_directions),
    JumpGrids = JPSMod:identity_successors(EndGrid, ValidFun, VisitedGrids, StartGrid, parent),
    {OpenGrids1, VisitedGrids1} = add_jump_grids(EndGrid, JPSMod, StartGrid, G, [StartGrid], OpenGrids, VisitedGrids, JumpGrids),
    MaxLimit = proplists:get_value(max_limit, Options, 16#FFFF),
    do_search(EndGrid, ValidFun, JPSMod, OpenGrids1, VisitedGrids1, MaxLimit).

-spec get_full_path(JumpPoints :: [grid()]) -> {full_path, Path :: [grid()]}.
get_full_path(JumpPoints) ->
    Path = get_full_path_1(JumpPoints, []),
    {full_path, lists:reverse(Path)}.

%%%======================================================================
%%% Internal Functions
%%%======================================================================
do_search(EndGrid, ValidFun, JPSMod, OpenGrids, VisitedGrids, MaxLimit) when MaxLimit > 0 ->
%%    jps_test:draw_map(OpenGrids),
    case gb_trees:is_empty(OpenGrids) of
        true ->
            none;
        false ->
            case gb_trees:take_smallest(OpenGrids) of
                {{_Score, EndGrid}, {_G, ParentPath}, _OpenGrids1} ->
                    {jump_points, lists:reverse([EndGrid | ParentPath])};
                {{_Score, Grid}, {G, ParentPath}, OpenGrids1} ->
%%                    io:format("take_grid Grid:~w ParentGrid:~w G:~w~n~n", [Grid, hd(ParentPath), G]),
                    VisitedGrids1 = VisitedGrids#{Grid := -1},
                    JumpGrids = JPSMod:identity_successors(EndGrid, ValidFun, VisitedGrids1, Grid, hd(ParentPath)),
                    {OpenGrids2, VisitedGrids2} = add_jump_grids(EndGrid, JPSMod, Grid, G, [Grid | ParentPath], OpenGrids1, VisitedGrids1, JumpGrids),
                    do_search(EndGrid, ValidFun, JPSMod, OpenGrids2, VisitedGrids2, MaxLimit - 1)
            end
    end;
do_search(_EndGrid, _ValidFun, _JPSMod, _OpenGrids, _VisitedGrids, _MaxLimit) ->
    max_limited.

add_jump_grids(EndGrid, JPSMod, ParentGrid, G, Path, OpenGrids, VisitedGrids, [Grid | T]) ->
    G1 = G + JPSMod:g(Grid, ParentGrid),
    Score = G1 + JPSMod:h(Grid, EndGrid),
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
    add_jump_grids(EndGrid, JPSMod, ParentGrid, G, Path, OpenGrids2, VisitedGrids1, T);
add_jump_grids(_EndGrid, _JPSMod, _ParentGrid, _G, _Path, OpenGrids, VisitedGrids, []) ->
    {OpenGrids, VisitedGrids}.

get_full_path_1([Grid1, Grid2 | T], Path) ->
    {DX, DY} = jps_util:get_direction(Grid2, Grid1),
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

%%%======================================================================
%%% Tests Functions
%%%======================================================================
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
    case jps:search(Start, End, Fun, [{jps_mod, jps_eight_directions}]) of
        {jump_points, Path} ->
            {full_path, FullPath} = jps:get_full_path(Path);
        _Other ->
            FullPath = []
    end,
    IsShow andalso draw_map(Row, FullPath, WorldMap),
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