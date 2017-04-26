-module(emq_persistence_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, Sup} = emq_persistence_sup:start_link(),
    emq_n2o:load([]),
    {ok, Sup}.

stop(_State) ->
    emq_kvs_bridge:unload().

