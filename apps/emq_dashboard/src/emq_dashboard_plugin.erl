%%--------------------------------------------------------------------
%% Copyright (c) 2015-2017 EMQ Enterprise, Inc. (http://emqtt.io).
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

%% @doc emqttd Plugins API.

-module(emq_dashboard_plugin).

-include_lib("emqttd/include/emqttd.hrl").

-export([plugins/0,
         enable/1,
         disable/1]).

-http_api({"plugins",  plugins,   []}).

-http_api({"enable",  enable,   [{"plugin_name", atom}]}).

-http_api({"disable",  disable,   [{"plugin_name", atom}]}).

plugins() ->
    Plugins = lists:map(fun plugin/1, emqttd_plugins:list()),
    {ok, Plugins}.

plugin(#mqtt_plugin{name = Name, version = Ver, descr = Descr,
                    active = Active}) ->
    [{name, Name},
     {version, iolist_to_binary(Ver)},
     {description, iolist_to_binary(Descr)},
     {active, Active}].

enable(PluginName) ->
    case emqttd_plugins:load(PluginName) of
        {ok, _StartedApp} ->
            {ok, [{active, true}]};
        {error, _Reason} ->
            {ok, [{active, false}]}
    end.

disable(PluginName) ->
    case emqttd_plugins:unload(PluginName) of
        ok ->
            {ok, [{active, true}]};
        {error, _Reason} ->
            {ok, [{active, false}]}
    end.
