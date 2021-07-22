%%%-------------------------------------------------------------------
%%% @author gz1417
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 8方向跳点寻路
%%% @end
%%% Created : 19. 7月 2021 15:31
%%%-------------------------------------------------------------------
-module(jps_eight_directions).

-behavior(jps).

-include("jps.hrl").

%% Callbacks API
-export([identity_successors/5, g/2, h/2]).

-spec identity_successors(EndGrid, ValidFun, VisitedGrids, CurGrid, ParentGrid) -> JumpPoints when
    EndGrid :: jps:grid(), ValidFun :: jps:valid_fun(), VisitedGrids :: jps:visited_grids(),
    CurGrid :: jps:grid(), ParentGrid :: jps:grid(), JumpPoints :: [jps:grid()].
identity_successors(EndGrid, ValidFun, VisitedGrids, CurGrid, ParentGrid) ->
    Directions = directions(CurGrid, ParentGrid),
    do_jump_points(EndGrid, ValidFun, VisitedGrids, CurGrid, Directions).

directions(_CurGrid, parent) ->
    [{1, 0}, {1, 1}, {0, 1}, {-1, 1}, {-1, 0}, {-1, -1}, {0, -1}, {1, -1}];
directions(CurGrid, ParentGrid) ->
    case jps_util:get_direction(CurGrid, ParentGrid) of
        {DX, 0} ->
            [{DX, 0}, {DX, 1}, {DX, -1}];
        {0, DY} ->
            [{0, DY}, {1, DY}, {-1, DY}];
        {DX, DY} ->
            [{0, DY}, {DX, 0}, {DX, DY}, {-DX, DY}, {DX, -DY}]
    end.

do_jump_points(EndGrid, ValidFun, VisitedGrids, CurGrid, [{DX, DY} | T]) ->
    case do_jump_gird(EndGrid, ValidFun, VisitedGrids, CurGrid, DX, DY) of
        none ->
            do_jump_points(EndGrid, ValidFun, VisitedGrids, CurGrid, T);
        NeighbourGrid ->
            [NeighbourGrid | do_jump_points(EndGrid, ValidFun, VisitedGrids, CurGrid, T)]
    end;
do_jump_points(_EndGrid, _ValidFun, _VisitedGrids, _CurGrid, []) ->
    [].

do_jump_gird(EndGrid, ValidFun, VisitedGrids, {X, Y}, DX, DY) ->
    case {X + DX, Y + DY} of
        EndGrid ->
            ?IF(ValidFun(EndGrid), EndGrid, none);
        {X1, Y1} = NeighbourGrid ->
            case jps_util:is_open(NeighbourGrid, VisitedGrids) andalso ValidFun(NeighbourGrid) of
                true when DX =/= 0, DY =/= 0 ->
                    ?IF(ValidFun({X1, Y}) orelse ValidFun({X, Y1}),
                        jump_grid_1(EndGrid, ValidFun, VisitedGrids, NeighbourGrid, DX, DY), none);
                true ->
                    ?IF(ValidFun({X1 + DX, Y1 + DY}),
                        jump_grid_1(EndGrid, ValidFun, VisitedGrids, NeighbourGrid, DX, DY), none);
                false ->
                    none
            end
    end.

jump_grid_1(EndGrid, ValidFun, VisitedGrids, NeighbourGrid, DX, DY) when DX =:= 0; DY =:= 0 ->
    case check_jump_grid(ValidFun, NeighbourGrid, DX, DY) of
        true ->
            NeighbourGrid;
        false ->
            do_jump_gird(EndGrid, ValidFun, VisitedGrids, NeighbourGrid, DX, DY)
    end;
jump_grid_1(EndGrid, ValidFun, VisitedGrids, NeighbourGrid, DX, DY) ->
    case check_jump_grid(ValidFun, NeighbourGrid, DX, DY)
        orelse do_jump_gird(EndGrid, ValidFun, VisitedGrids, NeighbourGrid, DX, 0) =/= none
        orelse do_jump_gird(EndGrid, ValidFun, VisitedGrids, NeighbourGrid, 0, DY) =/= none of
        true ->
            NeighbourGrid;
        false ->
            do_jump_gird(EndGrid, ValidFun, VisitedGrids, NeighbourGrid, DX, DY)
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

-spec g(Grid1 :: jps:grid(), Grid2 :: jps:grid()) -> G :: number().
g(Grid1, Grid2) ->
    jps_util:g(Grid1, Grid2).

-spec h(Grid1 :: jps:grid(), Grid2 :: jps:grid()) -> H :: number().
h(Grid1, Grid2) ->
    jps_util:h(Grid1, Grid2).