jps
=====

Erlang实现的跳点寻路算法

Build
-----

    $ rebar3 compile

How To Use
----
    1> StartGrid = {1, 1}.
    2> EndGrid = {50, 50}.
    2> BlockList = [{47,1},{24,2}, {2,25}, {20,31}, {20,21}, {50,20}, ...].
    3> ValidFun = fun({X,Y}) -> not lists:member({X,Y}, BlockList) end.
    4> Options = [],
    5> {jump_points, JumpPoints} = jps:search(StartGrid, EndGrid, ValidFun, Options).
    6> {full_path, Path} = get_full_path(JumpPoints).
