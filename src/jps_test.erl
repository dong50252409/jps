%%%-------------------------------------------------------------------
%%% @author gz1417
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 7月 2021 11:47
%%%-------------------------------------------------------------------
-module(jps_test).

-ifdef(TEST).

%% API
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