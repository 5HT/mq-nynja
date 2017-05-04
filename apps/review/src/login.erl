-module(login).
-compile({parse_transform, lager_transform}).
-compile(export_all).
-include_lib("kvs/include/feed.hrl").
-include_lib("n2o/include/n2o.hrl").
-include_lib("nitro/include/nitro.hrl").

event(init) ->
    nitro:update(loginButton,
          #button { id=loginButton, body="Login",
                    postback=login,source=[user,pass]});

event(login) ->
    User = case n2o:q(user) of undefined -> "anonymous";
                              E -> nitro:to_list(E) end,
    n2o:user(User),
    lager:info("User: ~p",[n2o:user()]),
    nitro:redirect("index.htm?room="++nitro:to_list(n2o:q(pass)));

event(_) -> [].
main()   -> [].
