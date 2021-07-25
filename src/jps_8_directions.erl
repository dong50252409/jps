%%%-------------------------------------------------------------------
%%% @author gz1417
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 8方向跳点寻路
%%% @end
%%% Created : 19. 7月 2021 15:31
%%%-------------------------------------------------------------------
-module(jps_8_directions).

-behavior(jps).

-include("jps.hrl").

%% Callbacks API
-export([identity_successors/4, g/2, h/2]).

-spec identity_successors(EndGrid, ValidFun, CurGrid, ParentGrid) -> JumpPoints when
    EndGrid :: jps:grid(), ValidFun :: jps:valid_fun(),
    CurGrid :: jps:grid(), ParentGrid :: jps:grid(), JumpPoints :: [jps:grid()].
identity_successors(EndGrid, ValidFun, CurGrid, ParentGrid) ->
    Neighbours = get_neighbours(ValidFun, CurGrid, ParentGrid),
    find_jump_grids(EndGrid, ValidFun, CurGrid, Neighbours).

get_neighbours(ValidFun, {X, Y}, parent) ->
    Up = ValidFun({X, Y + 1}), Down = ValidFun({X, Y - 1}),
    Left = ValidFun({X - 1, Y}), Right = ValidFun({X + 1, Y}),
    UpLeft = ValidFun({X - 1, Y + 1}), UpRight = ValidFun({X + 1, Y + 1}),
    DownLeft = ValidFun({X - 1, Y - 1}), DownRight = ValidFun({X + 1, Y - 1}),
    ?IF(Up, [{X, Y + 1}], []) ++ ?IF(Down, [{X, Y - 1}], [])
        ++ ?IF(Left, [{X - 1, Y}], []) ++ ?IF(Right, [{X + 1, Y}], [])
        ++ ?IF(Up andalso Left andalso UpLeft, [{X - 1, Y + 1}], [])
        ++ ?IF(Up andalso Right andalso UpRight, [{X + 1, Y + 1}], [])
        ++ ?IF(Down andalso Left andalso DownLeft, [{X - 1, Y - 1}], [])
        ++ ?IF(Down andalso Right andalso DownRight, [{X + 1, Y - 1}], []);
get_neighbours(ValidFun, {X, Y} = Grid, Parent) ->
    case jps_util:get_direction(Grid, Parent) of
        {DX, 0} ->
            ?IF(ValidFun({X + DX, Y}), [{X + DX, Y}], [])
            ++ ?IF(ValidFun({X, Y + 1}), [], [{X + DX, Y + 1}])
                ++ ?IF(ValidFun({X, Y - 1}), [], [{X + DX, Y - 1}]);
        {0, DY} ->
            ?IF(ValidFun({X, Y + DY}), [{X, Y + DY}], [])
            ++ ?IF(ValidFun({X + 1, Y}), [], [{X + 1, Y + DY}])
                ++ ?IF(ValidFun({X - 1, Y}), [], [{X - 1, Y + DY}]);
        {DX, DY} ->
            ?IF(ValidFun({X, Y + DY}), [{X, Y + DY}], [])
            ++ ?IF(ValidFun({X + DX, Y}), [{X + DX, Y}], [])
                ++ ?IF(ValidFun({X + DX, Y + DY}), [{X + DX, Y + DY}], [])
                ++ ?IF(ValidFun({X - DX, Y}), [], [{X - DX, Y + DY}])
                ++ ?IF(ValidFun({X, Y + DY}), [], [{X + DX, Y - DY}])
    end.

find_jump_grids(EndGrid, ValidFun, ParentGrid, [Grid | T]) ->
    case find_jump_grid(EndGrid, ValidFun, ParentGrid, Grid) of
        none ->
            find_jump_grids(EndGrid, ValidFun, ParentGrid, T);
        NeighbourGrid ->
            [NeighbourGrid | find_jump_grids(EndGrid, ValidFun, ParentGrid, T)]
    end;
find_jump_grids(_EndGrid, _ValidFun, _CurGrid, []) ->
    [].

find_jump_grid(EndGrid, ValidFun, _ParentGrid, EndGrid) ->
    ?IF(ValidFun(EndGrid), EndGrid, none);
find_jump_grid(_EndGrid, ValidFun, ParentGrid, {X, Y} = Grid) ->
    case ValidFun(Grid) of
        true ->
            case jps_util:get_direction(Grid, ParentGrid) of
                {DX, 0} ->
                    case not ValidFun({X, Y + 1}) andalso ValidFun({X + DX, Y + 1})
                        orelse not ValidFun({X, Y - 1}) andalso ValidFun({X + DX, Y - 1}) of
                        true ->
                            Grid;
                        false ->
                            find_jump_grid(_EndGrid, ValidFun, Grid, {X + DX, Y})
                    end;
                {0, DY} ->
                    case not ValidFun({X + 1, Y}) andalso ValidFun({X + 1, Y + DY})
                        orelse not ValidFun({X - 1, Y}) andalso ValidFun({X - 1, Y + DY}) of
                        true ->
                            Grid;
                        false ->
                            find_jump_grid(_EndGrid, ValidFun, Grid, {X, Y + DY})
                    end;
                {DX, DY} ->
                    case (not ValidFun({X - DX, Y}) andalso ValidFun({X - DX, Y + DY}))
                        orelse (not ValidFun({X, Y - DY}) andalso ValidFun({X + DX, Y - DY}))
                        orelse find_jump_grid(_EndGrid, ValidFun, Grid, {X + DX, Y}) =/= none
                        orelse find_jump_grid(_EndGrid, ValidFun, Grid, {X, Y + DY}) =/= none of
                        true ->
                            Grid;
                        false ->
                            find_jump_grid(_EndGrid, ValidFun, Grid, {X + DX, Y + DY})
                    end
            end;
        false ->
            none
    end.

-spec g(Grid1 :: jps:grid(), Grid2 :: jps:grid()) -> G :: number().
g(Grid1, Grid2) ->
    jps_heuristic:octile(Grid1, Grid2).

-spec h(Grid1 :: jps:grid(), Grid2 :: jps:grid()) -> H :: number().
h(Grid1, Grid2) ->
    jps_heuristic:euclidean(Grid1, Grid2).