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
-export([identity_successors/4, g/2, h/2]).

-spec identity_successors(EndGrid, ValidFun, CurGrid, ParentGrid) -> JumpPoints when
    EndGrid :: jps:grid(), ValidFun :: jps:valid_fun(),
    CurGrid :: jps:grid(), ParentGrid :: jps:grid(), JumpPoints :: [jps:grid()].
identity_successors(EndGrid, ValidFun, CurGrid, ParentGrid) ->
    Directions = directions(ValidFun, CurGrid, ParentGrid),
    get_neighbour_girds(EndGrid, ValidFun, CurGrid, Directions).

directions(_ValidFun, _CurGrid, parent) ->
    [{1, 0}, {1, 1}, {0, 1}, {-1, 1}, {-1, 0}, {-1, -1}, {0, -1}, {1, -1}];
directions(ValidFun, {X, Y} = CurGrid, ParentGrid) ->
    case jps_util:get_direction(CurGrid, ParentGrid) of
        {DX, 0} ->
            Directions = [{DX, 0}],
            Directions1 = ?IF(ValidFun({X, Y + 1}), Directions, [{DX, 1} | Directions]),
            ?IF(ValidFun({X, Y - 1}), Directions1, [{DX, -1} | Directions]);
        {0, DY} ->
            Directions = [{0, DY}],
            Directions1 = ?IF(ValidFun({X + 1, Y}), Directions, [{1, DY} | Directions]),
            ?IF(ValidFun({X - 1, Y}), Directions1, [{-1, DY} | Directions1]);
        {DX, DY} ->
            Directions = [{0, DY}, {DX, 0}, {DX, DY}],
            Directions1 = ?IF(ValidFun({X - DX, Y}), Directions, [{-DX, DY} | Directions]),
            ?IF(ValidFun({X, Y - DY}), Directions1, [{DX, -DY} | Directions1])
    end.

get_neighbour_girds(EndGrid, ValidFun, CurGrid, [{DX, DY} | T]) ->
    case get_neighbour_gird(EndGrid, ValidFun, CurGrid, DX, DY) of
        none ->
            get_neighbour_girds(EndGrid, ValidFun, CurGrid, T);
        NeighbourGrid ->
            [NeighbourGrid | get_neighbour_girds(EndGrid, ValidFun, CurGrid, T)]
    end;
get_neighbour_girds(_EndGrid, _ValidFun, _CurGrid, []) ->
    [].

get_neighbour_gird(EndGrid, ValidFun, {X, Y}, DX, DY) ->
    case {X + DX, Y + DY} of
        EndGrid ->
            ?IF(ValidFun(EndGrid), EndGrid, none);
        {X1, Y1} = NeighbourGrid ->
            case ValidFun(NeighbourGrid) of
                true when DY =:= 0 ->
                    ?IF(ValidFun({X1 + DX, Y1}),
                        horizontal(EndGrid, ValidFun, NeighbourGrid, DX), none);
                true when DX =:= 0 ->
                    ?IF(ValidFun({X1, Y1 + DY}),
                        vertical(EndGrid, ValidFun, NeighbourGrid, DY), none);
                true ->
                    ?IF(ValidFun({X1, Y}) orelse ValidFun({X, Y1}),
                        diagonally(EndGrid, ValidFun, NeighbourGrid, DX, DY), none);
                false ->
                    none
            end
    end.

horizontal(EndGrid, ValidFun, {X, Y} = NeighbourGrid, DX) ->
    case not ValidFun({X, Y + 1}) andalso ValidFun({X + DX, Y + 1})
        orelse not ValidFun({X, Y - 1}) andalso ValidFun({X + DX, Y - 1}) of
        true ->
            NeighbourGrid;
        false ->
            get_neighbour_gird(EndGrid, ValidFun, NeighbourGrid, DX, 0)
    end.

vertical(EndGrid, ValidFun, {X,Y} = NeighbourGrid, DY) ->
    case not ValidFun({X + 1, Y}) andalso ValidFun({X + 1, Y + DY})
        orelse not ValidFun({X - 1, Y}) andalso ValidFun({X - 1, Y + DY}) of
        true ->
            NeighbourGrid;
        false ->
            get_neighbour_gird(EndGrid, ValidFun, NeighbourGrid, 0, DY)
    end.

diagonally(EndGrid, ValidFun, {X,Y} = NeighbourGrid, DX, DY) ->
    case (not ValidFun({X - DX, Y}) andalso ValidFun({X, Y + DY}) andalso ValidFun({X - DX, Y + DY}))
        orelse (not ValidFun({X, Y - DY}) andalso ValidFun({X + DX, Y}) andalso ValidFun({X + DX, Y - DY}))
        orelse get_neighbour_gird(EndGrid, ValidFun, NeighbourGrid, DX, 0) =/= none
        orelse get_neighbour_gird(EndGrid, ValidFun, NeighbourGrid, 0, DY) =/= none of
        true ->
            NeighbourGrid;
        false ->
            get_neighbour_gird(EndGrid, ValidFun, NeighbourGrid, DX, DY)
    end.

-spec g(Grid1 :: jps:grid(), Grid2 :: jps:grid()) -> G :: number().
g(Grid1, Grid2) ->
    jps_heuristic:octile(Grid1, Grid2).

-spec h(Grid1 :: jps:grid(), Grid2 :: jps:grid()) -> H :: number().
h(Grid1, Grid2) ->
    jps_heuristic:euclidean(Grid1, Grid2).