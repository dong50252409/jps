-module(jps_test).

%% API
-export([test/4]).
-export([gen_map/3]).

test(Row, Col, BlockNum, Times) ->
    test_1({1, 1}, [{Row, 1}, {1, Col}, {Row, Col}], Row, Col, BlockNum, Times).

test_1(Start, EndList, Row, Col, BlockNum, Times) when Times > 0 ->
    WorldMap = gen_map(Row, Col, BlockNum),
    Fun =
        fun({X, Y}) ->
            X > 0 andalso X =< Row andalso Y > 0 andalso Y =< Col
                andalso element(X, element(Y, WorldMap)) =/= $X
        end,
    put(map, WorldMap),
    try
        lists:foreach(
            fun(End) ->
                io:format("Start:~w End:~w~n", [Start, End]),
                case jps:search(Start, End, Fun, []) of
                    {jump_points, Path} ->
%%                io:format("PointPath:~w~n", [Path]),
%%                draw_map(Row, Path, WorldMap),
                        {full_path, FullPath} = jps:get_full_path(Path),
                        io:format("FullPath:~w~n", [FullPath]),
                        draw_map(Row, FullPath, WorldMap);
                    Other ->
                        io:format("~w~n", [WorldMap]),
                        draw_map(Row, [], WorldMap),
                        Other
                end
            end,
            EndList),
        test_1(Start, EndList, Row, Col, BlockNum, Times - 1)
    catch
        Err:Reason:Trace ->
            io:format("~w~n", [WorldMap]),
            {Err, Reason, Trace}
    end;
test_1(_Start, _End, _Row, _Col, _BlockNum, _Times) ->
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
    io:format("~s~n", [[Border, Str, Border]]).

draw_map_path([{X, Y} | T], WorldMap) ->
    WorldMap1 = setelement(Y, WorldMap, setelement(X, element(Y, WorldMap), $o)),
    draw_map_path(T, WorldMap1);
draw_map_path(_, WorldMap) ->
    WorldMap.