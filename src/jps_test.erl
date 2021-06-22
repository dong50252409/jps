-module(jps_test).

%% API
-export([gen_map/3]).

gen_map(Row, Col, BlockNum) ->
    BlockSets = gen_block(Row, Col, BlockNum, sets:new()),
    draw_map(Row, Col, BlockSets).

gen_block(Row, Col, BlockNum, BlockSets) when BlockNum > 0 ->
    BlockGrid = {rand:uniform(Row), rand:uniform(Col)},
    case sets:is_element(BlockGrid, BlockSets) of
        true ->
            gen_block(Row, Col, BlockNum, BlockSets);
        false ->
            gen_block(Row, Col, BlockNum - 1, sets:add_element(BlockGrid, BlockSets))
    end;
gen_block(_Row, _Col, _BlockNum, BlockSets) ->
    BlockSets.

draw_map(Row, Col, BlockSets) ->
    Border = lists:duplicate(Row, "X") ++ "X\n",
    io:format("~ts", [["X", Border | [draw_map_1(Row, Col, BlockSets) | Border]]]),
    BlockSets.

draw_map_1(Row, Col, BlockSets) when Col > 0 ->
    ["X", draw_map_2(Row, Col, BlockSets) | draw_map_1(Row, Col - 1, BlockSets)];
draw_map_1(_Row, _Col, _BlockSets) ->
    ["X"].

draw_map_2(Row, Col, BlockSets) when Row > 0 ->
    case sets:is_element({Row, Col}, BlockSets) of
        true ->
            [$X | draw_map_2(Row - 1, Col, BlockSets)];
        false ->
            [" " | draw_map_2(Row - 1, Col, BlockSets)]
    end;
draw_map_2(_Row, _Col, _BlockSets) ->
    ["X\n"].