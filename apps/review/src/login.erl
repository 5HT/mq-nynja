-module(login).
-compile(export_all).
-include_lib("kvs/include/feed.hrl").
-include_lib("nitro/include/nitro.hrl").
-include_lib("n2o/include/wf.hrl").

main() -> [].

event(init) ->
    wf:update(loginButton, #button { id=loginButton,
                                     body="Login",
                                     postback=login,source=[user,pass]});

event(login) ->
    User = case wf:q(user) of <<>> -> "anonymous";
                              undefined -> "anonymous";
                              E -> wf:to_list(E) end,
    wf:user(User),
    wf:info(?MODULE,"User: ~p",[wf:user()]),
    wf:redirect("index.htm?room="++wf:to_list(wf:q(pass))),
    ok;

event(_) -> [].
