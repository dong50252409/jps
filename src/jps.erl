%%%-------------------------------------------------------------------
%%% @author gz1417
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 跳点寻路
%%% @end
%%% Created : 19. 7月 2021 11:47
%%%-------------------------------------------------------------------
-module(jps).

-include("jps.hrl").

%% API
-export([search/4, get_full_path/1]).

-export_type([grid/0, direction/0, valid_fun/0, visited_grids/0, result/0]).

-type grid() :: {X :: non_neg_integer(), Y :: non_neg_integer()}.
-type direction() :: {-1|0|1, -1|0|1}.
-type valid_fun() :: fun((grid()) -> boolean()).
-type visited_grids() :: #{JumpPointGrid :: grid() => -1 | non_neg_integer()}.
-type result() :: {jump_points, [grid()]}|none|max_limited.
-type max_limit() :: {max_limit, non_neg_integer()}.
-type option() :: max_limit() |{jps_mod, module()}.
-type options() :: [option()].

-callback(identity_successors(EndGrid :: grid(), ValidFun :: valid_fun(), VisitedGrids :: visited_grids(),
    CurGrid :: grid(), ParentPath :: [grid()]) -> JumpPoints :: [grid()]).
-callback(g(Grid1 :: grid(), Grid2 :: grid()) -> G :: number()).
-callback(h(Grid1 :: grid(), Grid2 :: grid()) -> H :: number()).
%%%======================================================================
%%% API Functions
%%%======================================================================
-spec search(StartGrid :: grid(), EndGrid :: grid(), ValidFun :: valid_fun(), Options :: options()) -> result().
search(StartGrid, EndGrid, ValidFun, Options) ->
    G = 0,
    OpenGrids = gb_trees:empty(),
    VisitedGrids = #{StartGrid => -1},
    JPSMod = proplists:get_value(jps_mod, Options, jps_eight_directions),
    JumpGrids = JPSMod:identity_successors(EndGrid, ValidFun, VisitedGrids, StartGrid, parent),
    {OpenGrids1, VisitedGrids1} = add_jump_grids(EndGrid, JPSMod, StartGrid, G, [StartGrid], OpenGrids, VisitedGrids, JumpGrids),
    MaxLimit = proplists:get_value(max_limit, Options, 16#FFFF),
    do_search(EndGrid, ValidFun, JPSMod, OpenGrids1, VisitedGrids1, MaxLimit).

-spec get_full_path(JumpPoints :: [grid()]) -> {full_path, Path :: [grid()]}.
get_full_path(JumpPoints) ->
    Path = get_full_path_1(JumpPoints, []),
    {full_path, lists:reverse(Path)}.

%%%======================================================================
%%% Internal Functions
%%%======================================================================
do_search(EndGrid, ValidFun, JPSMod, OpenGrids, VisitedGrids, MaxLimit) when MaxLimit > 0 ->
%%    jps_test:draw_map(OpenGrids),
    case gb_trees:is_empty(OpenGrids) of
        true ->
            none;
        false ->
            case gb_trees:take_smallest(OpenGrids) of
                {{_Score, EndGrid}, {_G, ParentPath}, _OpenGrids1} ->
                    {jump_points, lists:reverse([EndGrid | ParentPath])};
                {{_Score, Grid}, {G, ParentPath}, OpenGrids1} ->
%%                    io:format("take_grid Grid:~w ParentGrid:~w G:~w~n~n", [Grid, hd(ParentPath), G]),
                    VisitedGrids1 = VisitedGrids#{Grid := -1},
                    JumpGrids = JPSMod:identity_successors(EndGrid, ValidFun, VisitedGrids1, Grid, hd(ParentPath)),
                    {OpenGrids2, VisitedGrids2} = add_jump_grids(EndGrid, JPSMod, Grid, G, [Grid | ParentPath], OpenGrids1, VisitedGrids1, JumpGrids),
                    do_search(EndGrid, ValidFun, JPSMod, OpenGrids2, VisitedGrids2, MaxLimit - 1)
            end
    end;
do_search(_EndGrid, _ValidFun, _JPSMod, _OpenGrids, _VisitedGrids, _MaxLimit) ->
    max_limited.

add_jump_grids(EndGrid, JPSMod, ParentGrid, G, Path, OpenGrids, VisitedGrids, [Grid | T]) ->
    G1 = G + JPSMod:g(Grid, ParentGrid),
    Score = G1 + JPSMod:h(Grid, EndGrid),
    case VisitedGrids of
        #{Grid := OldScore} when OldScore =< Score ->
            OpenGrids2 = OpenGrids,
            VisitedGrids1 = VisitedGrids;
        #{Grid := OldScore} when OldScore > Score ->
%%            io:format("replace_add_grids OldScore:~w Score:~w Grid:~w G1:~w~n", [OldScore, Score, Grid, G1]),
            OpenGrids1 = gb_trees:delete({OldScore, Grid}, OpenGrids),
            OpenGrids2 = gb_trees:insert({Score, Grid}, {G1, Path}, OpenGrids1),
            VisitedGrids1 = VisitedGrids#{Grid => Score};
        _ ->
%%            io:format("add_grids Score:~w Grid:~w G1:~w~n", [Score, Grid, G1]),
            OpenGrids2 = gb_trees:insert({Score, Grid}, {G1, Path}, OpenGrids),
            VisitedGrids1 = VisitedGrids#{Grid => Score}
    end,
    add_jump_grids(EndGrid, JPSMod, ParentGrid, G, Path, OpenGrids2, VisitedGrids1, T);
add_jump_grids(_EndGrid, _JPSMod, _ParentGrid, _G, _Path, OpenGrids, VisitedGrids, []) ->
    {OpenGrids, VisitedGrids}.

get_full_path_1([Grid1, Grid2 | T], Path) ->
    {DX, DY} = util:get_direction(Grid2, Grid1),
    Path1 = get_full_path_2(Grid1, Grid2, DX, DY, Path),
    get_full_path_1([Grid2 | T], Path1);
get_full_path_1([_], Path) ->
    Path.

get_full_path_2({X, Y}, Grid2, DX, DY, Path) ->
    case {X + DX, Y + DY} of
        Grid2 ->
            [Grid2 | Path];
        Grid ->
            get_full_path_2(Grid, Grid2, DX, DY, [Grid | Path])
    end.