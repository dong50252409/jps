-module(jps_test).

%% API
-export([gen_world_maps/4, test/3]).

gen_world_maps(Row, Col, BlockNum, Times) ->
    WorldMaps = [gen_map(Row, Col, BlockNum) || _ <- lists:seq(1, Times)],
    file:write_file("world_maps.data", term_to_binary(WorldMaps)).

test(Row, Col, IsShow) ->
    {ok, Data} = file:read_file("world_maps.data"),
    WorldMaps = binary_to_term(Data),
    {ok, IO} = file:open("world_maps.out", [write]),
    put(io, IO),
    {Time, _Value} = timer:tc(fun() -> test_1({1, 1}, {Row, Col}, Row, Col, WorldMaps, IsShow) end),
    file:close(IO),
    Time.

test_1(Start, End, Row, Col, [WorldMap | T], IsShow) ->
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
    case IsShow of
        true ->
            draw_map(Row, FullPath, WorldMap);
        false ->
            ok
    end,
    test_1(Start, End, Row, Col, T, IsShow);
test_1(_Start, _End, _Row, _Col, [], _IsShow) ->
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