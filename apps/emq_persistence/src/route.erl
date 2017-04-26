-module(route).
-compile(export_all).

select(Topic) ->
    [Module,Room] = case string:tokens(binary_to_list(Topic),"_") of
         [M,D] -> [M,D];
           [M] -> [M,"lobby"];
            [] -> ["index","lobby"] end, select(Module,Room).

select("index",Room) -> index;
select("login",Room) -> login.

