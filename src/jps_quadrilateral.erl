%%%-------------------------------------------------------------------
%%% @author gz1417
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 四边形跳点寻路
%%% @end
%%% Created : 19. 7月 2021 15:31
%%%-------------------------------------------------------------------
-module(jps_orthogonal).

-behavior(jps).

-include("jps.hrl").

%% Callbacks API
-export([identity_successors/4, g/2, h/2]).

-spec identity_successors(EndGrid, ValidFun, CurGrid, ParentGrid) -> JumpPoints when
    EndGrid :: jps:grid(), ValidFun :: jps:valid_fun(),
    CurGrid :: jps:grid(), ParentGrid :: jps:grid(), JumpPoints :: [jps:grid()].
identity_successors(EndGrid, ValidFun, CurGrid, ParentGrid) ->
    Directions = get_directions(CurGrid, ParentGrid),
    get_neighbour_grids(EndGrid, ValidFun, CurGrid, Directions).

get_directions(_CurGrid, parent) ->
    [{1, 0}, {-1, 0}, {0, 1}, {0, -1}];
get_directions(CurGrid, ParentGrid) ->
    case jps_util:get_direction(CurGrid, ParentGrid) of
        {DX, 0} ->
            [{0, 1}, {0, -1}, {DX, 0}];
        {0, DY} ->
            [{1, 0}, {-1, 0}, {0, DY}]
    end.

get_neighbour_grids(EndGrid, ValidFun, CurGrid, [{DX, DY} | T]) ->
    case get_neighbour_grid(EndGrid, ValidFun, CurGrid, DX, DY) of
        none ->
            get_neighbour_grids(EndGrid, ValidFun, CurGrid, T);
        NeighbourGrid ->
            [NeighbourGrid | get_neighbour_grids(EndGrid, ValidFun, CurGrid, T)]
    end;
get_neighbour_grids(_EndGrid, _ValidFun, _CurGrid, []) ->
    [].

get_neighbour_grid(EndGrid, ValidFun, {X, Y}, DX, DY) ->
    case {X + DX, Y + DY} of
        EndGrid ->
            ?IF(ValidFun(EndGrid), EndGrid, none);
        NeighbourGrid ->
            case ValidFun(NeighbourGrid) of
                true when DX =/= 0 ->
                    horizontal(EndGrid, ValidFun, NeighbourGrid, DX);
                true ->
                    vertical(EndGrid, ValidFun, NeighbourGrid, DY);
                false ->
                    none
            end
    end.

horizontal(EndGrid, ValidFun, {X, Y} = NeighbourGrid, DX) ->
    case not ValidFun({X - DX, Y + 1}) andalso ValidFun({X, Y + 1})
        orelse not ValidFun({X - DX, Y - 1}) andalso ValidFun({X, Y - 1}) of
        true ->
            NeighbourGrid;
        false ->
            get_neighbour_grid(EndGrid, ValidFun, NeighbourGrid, DX, 0)
    end.

vertical(EndGrid, ValidFun, {X, Y} = NeighbourGrid, DY) ->
    case (not ValidFun({X + 1, Y - DY}) andalso ValidFun({X + 1, Y})
        orelse not ValidFun({X - 1, Y - DY}) andalso ValidFun({X - 1, Y}))
        orelse get_neighbour_grid(EndGrid, ValidFun, NeighbourGrid, 1, 0) =/= none
        orelse get_neighbour_grid(EndGrid, ValidFun, NeighbourGrid, -1, 0) =/= none of
        true ->
            NeighbourGrid;
        false ->
            get_neighbour_grid(EndGrid, ValidFun, NeighbourGrid, 0, DY)
    end.


-spec g(Grid1 :: jps:grid(), Grid2 :: jps:grid()) -> G :: number().
g(Grid1, Grid2) ->
    jps_heuristic:manhattan(Grid1, Grid2).

-spec h(Grid1 :: jps:grid(), Grid2 :: jps:grid()) -> H :: number().
h(Grid1, Grid2) ->
    jps_heuristic:manhattan(Grid1, Grid2).