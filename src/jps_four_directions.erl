%%%-------------------------------------------------------------------
%%% @author gz1417
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 4方向跳点寻路
%%% @end
%%% Created : 19. 7月 2021 15:31
%%%-------------------------------------------------------------------
-module(jps_four_directions).

-behavior(jps).

-include("jps.hrl").

%% Callbacks API
-export([identity_successors/5, g/2, h/2]).

-spec identity_successors(EndGrid, ValidFun, VisitedGrids, CurGrid, ParentGrid) -> JumpPoints when
    EndGrid :: jps:grid(), ValidFun :: jps:valid_fun(), VisitedGrids :: jps:visited_grids(),
    CurGrid :: jps:grid(), ParentGrid :: jps:grid(), JumpPoints :: [jps:grid()].
identity_successors(EndGrid, ValidFun, VisitedGrids, CurGrid, ParentGrid) ->
    Directions = get_neighbours(CurGrid, ParentGrid),
    do_jump_points(EndGrid, ValidFun, VisitedGrids, CurGrid, Directions).

get_neighbours(_CurGrid, parent) ->
    [{1, 0}, {-1, 0}, {0, 1}, {0, -1}];
get_neighbours(CurGrid, ParentGrid) ->
    case jps_util:get_direction(CurGrid, ParentGrid) of
        {DX, 0} ->
            [{0, 1}, {0, -1}, {DX, 0}];
        {0, DY} ->
            [{1, 0}, {-1, 0}, {0, DY}]
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
        NeighbourGrid ->
            ?IF(jps_util:is_open(NeighbourGrid, VisitedGrids) andalso ValidFun(NeighbourGrid),
                do_jump_gird_1(EndGrid, ValidFun, VisitedGrids, NeighbourGrid, DX, DY), none)
    end.

do_jump_gird_1(EndGrid, ValidFun, VisitedGrids, NeighbourGrid, DX, 0) ->
    case check_jump_grid(ValidFun, NeighbourGrid, DX, 0) of
        true ->
            NeighbourGrid;
        false ->
            do_jump_gird(EndGrid, ValidFun, VisitedGrids, NeighbourGrid, DX, 0)
    end;
do_jump_gird_1(EndGrid, ValidFun, VisitedGrids, NeighbourGrid, 0, DY) ->
    case check_jump_grid(ValidFun, NeighbourGrid, 0, DY)
        orelse do_jump_gird(EndGrid, ValidFun, VisitedGrids, NeighbourGrid, 1, 0) =/= none
        orelse do_jump_gird(EndGrid, ValidFun, VisitedGrids, NeighbourGrid, -1, 0) =/= none of
        true ->
            NeighbourGrid;
        false ->
            do_jump_gird(EndGrid, ValidFun, VisitedGrids, NeighbourGrid, 0, DY)
    end.

check_jump_grid(ValidFun, {X, Y}, DX, 0) ->
    not ValidFun({X - DX, Y + 1}) andalso ValidFun({X, Y + 1})
        orelse not ValidFun({X - DX, Y - 1}) andalso ValidFun({X, Y - 1});
check_jump_grid(ValidFun, {X, Y}, 0, DY) ->
    not ValidFun({X + 1, Y - DY}) andalso ValidFun({X + 1, Y})
        orelse not ValidFun({X - 1, Y - DY}) andalso ValidFun({X - 1, Y}).

-spec g(Grid1 :: jps:grid(), Grid2 :: jps:grid()) -> G :: number().
g(Grid1, Grid2) ->
    jps_util:g(Grid1, Grid2).

-spec h(Grid1 :: jps:grid(), Grid2 :: jps:grid()) -> H :: number().
h(Grid1, Grid2) ->
    jps_util:h(Grid1, Grid2).