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
    User = nitro:to_list(n2o:q(user)),
    Room = nitro:to_list(n2o:q(pass)),
    n2o:user(User),
    lager:info("User: ~p Pass: ~p~n",[n2o:user(),Room]),
    n2o:cache(room,Room),
    nitro:redirect("index.htm?room="++Room);

event(_) -> [].
main()   -> [].
