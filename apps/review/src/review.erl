-module(review).
-behaviour(supervisor).
-behaviour(application).
-export([init/1, start/2, stop/1, main/1]).
-include_lib("kvs/include/user.hrl").

main(A)    -> mad:main(A).
start()    -> start(normal,[]).
start(_,_) -> supervisor:start_link({local,review},review,[]).
stop(_)    -> ok.
init([])   -> application:set_env(n2o,session,n2o), kvs:join(),
              lager:set_loglevel(lager_console_backend, warning),
              {ok, {{one_for_one, 5, 10}, [spec()]}}.
spec()     -> ranch:child_spec(http, 100, ranch_tcp, port(), cowboy_protocol, env()).
env()      -> [ { env, [ { dispatch, points() } ] } ].
static()   ->   { dir, "apps/review/priv", mime() }.
n2o()      ->   { dir, "deps/n2o/priv",    mime() }.
mime()     -> [ { mimetypes, cow_mimetypes, all   } ].
port()     -> [ { port, application:get_env(n2o,port,8000)  } ].
points()   -> cowboy_router:compile([{'_', [
    { "/spa/[...]",          nitro_static,  static()},
    { "/n2o/[...]",          nitro_static,  n2o()},
    { "/rest/:resource",     rest_cowboy, []},
    { "/rest/:resource/:id", rest_cowboy, []} ]}]).
