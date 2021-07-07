-module(jps_test).

%% API
-export([test/3]).
-export([gen_map/3]).

test(Row, Col, BlockNum) ->
    WorldMap = gen_map(Row, Col, BlockNum),
    Fun =
        fun({X, Y}) ->
            X > 0 andalso X =< Row andalso Y > 0 andalso Y =< Col
                andalso element(X, element(Y, WorldMap)) =/= $X
        end,
%%    test_1({1, 1}, {Row, Col}, WorldMap, Fun, Row),
%%    test_1({1, 1}, {1, Col}, WorldMap, Fun, Row),
%%    test_1({1, 1}, {Row, 1}, WorldMap, Fun, Row).
    test_1({1, 1}, {Row, Col}, WorldMap, Fun, Row).

test_1(Start, End, WorldMap, Fun, Row) ->
    put(map, WorldMap),
    io:format("Start:~w End:~w~n", [Start, End]),
    try
        case jps_2:search(Start, End, Fun) of
            none ->
                io:format("~w~n", [WorldMap]),
                draw_map(Row, [], WorldMap),
                none;
            Path ->
                io:format("PointPath:~w~n", [Path]),
                draw_map(Row, Path, WorldMap),
                FullPath = jps_2:get_full_path(Start, Path),
                io:format("FullPath:~w~n", [FullPath]),
                draw_map(Row, FullPath, WorldMap)
        end
    catch
        Err:Reason ->
            io:format("~w~n", [WorldMap]),
            throw({Err, Reason})
    end.

gen_map(Row, Col, BlockNum) ->
    WorldMap = list_to_tuple(lists:duplicate(Col, list_to_tuple(lists:duplicate(Row, $ )))),
    gen_block(Row, Col, BlockNum, WorldMap).

gen_block(Row, Col, BlockNum, WorldMap) when BlockNum > 0 ->
    {X, Y} = {rand:uniform(Row), rand:uniform(Col)},
    C = element(Y, WorldMap),
    case element(X, C) =/= $X of
        true ->
            WorldMap1 = setelement(Y, WorldMap, setelement(X, C, $X)),
            gen_block(Row, Col, BlockNum - 1, WorldMap1);
        false ->
            gen_block(Row, Col, BlockNum, WorldMap)
    end;
gen_block(_Row, _Col, _BlockNum, WorldMap) ->
    WorldMap.

draw_map(Width, Path, WorldMap) ->
    WorldMap1 = draw_map_path(Path, WorldMap),
    Border = io_lib:format("~s~n", [lists:duplicate(Width + 2, $X)]),
    Str = [io_lib:format("X~sX~n", [tuple_to_list(Row)]) || Row <- tuple_to_list(WorldMap1)],
    io:format("~s~n", [[Border, Str, Border]]).

draw_map_path([{X, Y} | T], WorldMap) ->
    WorldMap1 = setelement(Y, WorldMap, setelement(X, element(Y, WorldMap), $o)),
    draw_map_path(T, WorldMap1);
draw_map_path(_, WorldMap) ->
    WorldMap.