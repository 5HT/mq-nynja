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

%% @doc emqttd alarms API.

-module(emq_dashboard_alarm).

-include_lib("emqttd/include/emqttd.hrl").

-export([alarms/0]).

-http_api({"alarms",  alarms,   []}).

alarms() ->
    Alarms = lists:map(fun alarm/1, emqttd_alarm:get_alarms()),
    {ok, Alarms}.

alarm(#mqtt_alarm{id        = AlarmId,
                  severity  = Severity,
                  title     = Title,
                  summary   = Summary,
                  timestamp = Timestamp}) ->
    [{id, AlarmId},
     {severity, Severity},
     {title, list_to_binary(Title)},
     {summary, list_to_binary(Summary)},
     {occurred_at, list_to_binary(emq_dashboard:strftime(Timestamp))}].

