%%%-------------------------------------------------------------------
%%% @author dy
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
-type option() :: max_limit() |{jps_mod, jps_quadrilateral|jps_polygonal}.
-type options() :: [option()].

-callback(identity_successors(EndGrid :: grid(), ValidFun :: valid_fun(),
    CurGrid :: grid(), ParentPath :: [grid()]) -> JumpPoints :: [grid()]).
-callback(g(Grid1 :: grid(), Grid2 :: grid()) -> G :: number()).
-callback(h(Grid1 :: grid(), Grid2 :: grid()) -> H :: number()).
%%%======================================================================
%%% API Functions
%%%======================================================================
-spec search(StartGrid :: grid(), EndGrid :: grid(), ValidFun :: valid_fun(), Options :: options()) -> result().
search(StartGrid, EndGrid, ValidFun, Options) ->
    case ValidFun(EndGrid) of
        true ->
            G = 0,
            OpenGrids = gb_trees:empty(),
            VisitedGrids = #{StartGrid => -1},
            JPSMod = proplists:get_value(jps_mod, Options, jps_diagonally),
            JumpGrids = JPSMod:identity_successors(EndGrid, ValidFun, StartGrid, parent),
%%            draw_map(JumpGrids),
            {OpenGrids1, VisitedGrids1} = add_jump_grids(EndGrid, JPSMod, StartGrid, G, [StartGrid], OpenGrids, VisitedGrids, JumpGrids),
            MaxLimit = proplists:get_value(max_limit, Options, 16#FFFF),
            do_search(EndGrid, ValidFun, JPSMod, OpenGrids1, VisitedGrids1, MaxLimit);
        false ->
            none
    end.

-spec get_full_path(JumpPoints :: [grid()]) -> {full_path, Path :: [grid()]}.
get_full_path(JumpPoints) ->
    Path = get_full_path_1(JumpPoints, []),
    {full_path, lists:reverse(Path)}.

%%%======================================================================
%%% Internal Functions
%%%======================================================================
do_search(EndGrid, ValidFun, JPSMod, OpenGrids, VisitedGrids, MaxLimit) when MaxLimit > 0 ->
    case gb_trees:is_empty(OpenGrids) of
        true ->
            none;
        false ->
            case gb_trees:take_smallest(OpenGrids) of
                {{_Score, EndGrid}, {_G, ParentPath}, _OpenGrids1} ->
                    {jump_points, lists:reverse([EndGrid | ParentPath])};
                {{_Score, Grid}, {G, ParentPath}, OpenGrids1} ->
                    VisitedGrids1 = VisitedGrids#{Grid := -1},
                    JumpGrids = JPSMod:identity_successors(EndGrid, ValidFun, Grid, hd(ParentPath)),
%%                    io:format("take_grid Grid:~w ParentGrid:~w G:~w~n~n", [Grid, hd(ParentPath), G]),
%%                    draw_map(JumpGrids),
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
    {DX, DY} = jps_util:get_direction(Grid2, Grid1),
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

%%%======================================================================
%%% Tests Functions
%%%======================================================================
-ifdef(TEST).

-compile(export_all).

test(Width, High, BlockNum, Num, IsShow) ->
    Mazes = [gen_map(Width, High, BlockNum) || _ <- lists:seq(1, Num)],
    Str = io_lib:format("{mazes,~w}.", [Mazes]),
    Str1 = string:replace(Str, <<"},">>, <<"},\n">>, all),
    file:write_file("mazes.data", Str1),
    test_1(IsShow, Mazes).

test(IsShow) ->
    {ok, Data} = file:consult("mazes.data"),
    Mazes = proplists:get_value(mazes, Data, []),
    test_1(IsShow, Mazes).

test_1(IsShow, Mazes) ->
    {ok, IO} = file:open("draw_mazes.out", [write]),
    put(io, IO),
    loop_mazes(Mazes, IsShow),
    file:close(IO).

loop_mazes([Maze | T], IsShow) ->
    put(maze_map, Maze),
    Width = size(element(1, Maze)),
    High = size(Maze),
    Fun =
        fun({X, Y}) ->
            X > 0 andalso X =< Width andalso Y > 0 andalso Y =< High
                andalso element(X, element(Y, Maze)) =/= 0
        end,
    case jps:search({1, 1}, {Width, High}, Fun, [{jps_mod, jps_eight_directions}]) of
        {jump_points, Path} ->
            {full_path, FullPath} = jps:get_full_path(Path);
        _Other ->
            FullPath = []
    end,
    case IsShow of
        true ->
            {ok, IO} = file:open("draw_mazes.out", [write]),
            draw_maze(IO, FullPath, Maze),
            file:close(IO);
        false ->
            ok
    end,
    loop_mazes(T, IsShow);
loop_mazes([], _IsShow) ->
    ok.

gen_map(Width, High, BlockNum) ->
    BlockGrids = gen_block(Width, High, BlockNum),
    Maze = list_to_tuple(lists:duplicate(High, list_to_tuple(lists:duplicate(Width, 1)))),
    fill_block(BlockGrids, Maze).

gen_block(Width, High, BlockNum) ->
    Grids = [{rand:uniform(), {X, Y}} || X <- lists:seq(1, Width), Y <- lists:seq(1, High)],
    Grids1 = [Grid || {_, Grid} <- lists:sort(Grids)],
    lists:sublist(Grids1, BlockNum).

fill_block([{X, Y} | T], Maze) ->
    Maze1 = setelement(Y, Maze, setelement(X, element(Y, Maze), 1)),
    fill_block(T, Maze1);
fill_block([], Maze) ->
    Maze.

draw_maze(IO, Path, Maze) ->
    Width = size(element(1, Maze)),
    Maze1 = draw_maze_path(Path, Maze),
    Border = io_lib:format("~s~n", [lists:duplicate(Width + 2, $X)]),
    Str = draw_maze(tuple_to_list(Maze1)),
    io:format(IO, "~s~n", [[Border, Str, Border]]).

draw_maze([Width | T]) ->
    [io_lib:format("X~sX~n", [lists:map(fun draw_maze_1/1, tuple_to_list(Width))]) | draw_maze(T)];
draw_maze([]) ->
    [].

draw_maze_1(0) ->
    $X;
draw_maze_1(1) ->
    " ";
draw_maze_1(E) ->
    E.

draw_maze_path([{X, Y} | T], Maze) ->
    Maze1 = setelement(Y, Maze, setelement(X, element(Y, Maze), $o)),
    draw_maze_path(T, Maze1);
draw_maze_path(_, Maze) ->
    Maze.

draw_map(JumpGrids) ->
    Map1 = draw_p(JumpGrids, get(maze_map)),
    put(maze_map, Map1),
    draw_maze(erlang:group_leader(), [], Map1).

draw_p([{X, Y} | T], Maze) ->
    draw_p(T, setelement(Y, Maze, setelement(X, element(Y, Maze), $P)));
draw_p([], Maze) ->
    Maze.
-endif.