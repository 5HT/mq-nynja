-module(route).
-compile(export_all).

select(Topic) ->
    [Module,Room] = case string:tokens(binary_to_list(iolist_to_binary(Topic)),"_") of
         [M,R] -> [M,R];
           [R] -> ["index",R];
            [] -> ["index","lobby"] end, select(Module,Room).

select("index",Room) -> index;
select("login",Room) -> login.

