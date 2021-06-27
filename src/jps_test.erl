-module(jps_test).

%% API
-export([test/3]).
-export([gen_map/3]).

test(Row, Col, BlockNum) ->
    WorldMap = gen_map(Row, Col, BlockNum),
    io:format("~w~n", [WorldMap]),
    Fun =
        fun({X, Y}) ->
            X > 0 andalso X =< Row
                andalso Y > 0 andalso Y =< Col
                andalso element(X, element(Y, WorldMap)) =/= $X
        end,
    Path = jps:search({1, 1}, {Row, Col}, Fun),
    draw_map(Row, Path, WorldMap).

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