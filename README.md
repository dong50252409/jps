jps
=====

Erlang实现的跳点寻路算法

构建 Build
-----

    $ rebar3 compile

如何使用 How To Use
----
    1> StartGrid = {1, 1}.
    2> EndGrid = {50, 50}.
    2> BlockList = [{47,1},{24,2}, {2,25}, {20,31}, {20,21}, {50,20}, ...].
    3> ValidFun = fun({X,Y}) -> not lists:member({X,Y}, BlockList) end.
    4> Options = [],
    5> {jump_points, JumpPoints} = jps:search(StartGrid, EndGrid, ValidFun, Options).
    6> {full_path, Path} = jps:get_full_path(JumpPoints).

如何扩展 How To Extend
-----
实现并导出 **identity_successors/5、g/2、h/2** 函数

参考：jps_eight_directions.erl、jps_four_directions.erl

