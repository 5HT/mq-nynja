%%--------------------------------------------------------------------
%% Copyright (c) 2013-2017 EMQ Enterprise, Inc. (http://emqtt.io)
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

-module(emq_mod_presence).

-behaviour(emqttd_gen_mod).

-author("Feng Lee <feng@emqtt.io>").

-include_lib("emqttd/include/emqttd.hrl").

-export([load/1, unload/1]).

-export([on_client_connected/3, on_client_disconnected/3]).

load(Env) ->
    emqttd:hook('client.connected',    fun ?MODULE:on_client_connected/3, [Env]),
    emqttd:hook('client.disconnected', fun ?MODULE:on_client_disconnected/3, [Env]).

on_client_connected(ConnAck, Client = #mqtt_client{client_id  = ClientId,
                                                   username   = Username,
                                                   peername   = {IpAddr, _},
                                                   clean_sess = CleanSess,
                                                   proto_ver  = ProtoVer}, Env) ->
    Payload = mochijson2:encode([{clientid, ClientId},
                                 {username, Username},
                                 {ipaddress, iolist_to_binary(emqttd_net:ntoa(IpAddr))},
                                 {clean_sess, CleanSess},
                                 {protocol, ProtoVer},
                                 {connack, ConnAck},
                                 {ts, emqttd_time:now_secs()}]),
    Msg = message(qos(Env), topic(connected, ClientId), Payload),
    emqttd:publish(emqttd_message:set_flag(sys, Msg)),
    {ok, Client}.

on_client_disconnected(Reason, #mqtt_client{client_id = ClientId}, Env) ->
    Payload = mochijson2:encode([{clientid, ClientId},
                                 {reason, reason(Reason)},
                                 {ts, emqttd_time:now_secs()}]),
    Msg = message(qos(Env), topic(disconnected, ClientId), Payload),
    emqttd:publish(emqttd_message:set_flag(sys, Msg)), ok.

unload(_Env) ->
    emqttd:unhook('client.connected',    fun ?MODULE:on_client_connected/3),
    emqttd:unhook('client.disconnected', fun ?MODULE:on_client_disconnected/3).

message(Qos, Topic, Payload) ->
    emqttd_message:make(presence, Qos, Topic, iolist_to_binary(Payload)).

topic(connected, ClientId) ->
    emqttd_topic:systop(list_to_binary(["clients/", ClientId, "/connected"]));
topic(disconnected, ClientId) ->
    emqttd_topic:systop(list_to_binary(["clients/", ClientId, "/disconnected"])).

qos(Env) -> proplists:get_value(qos, Env, 0).

reason(Reason) when is_atom(Reason) -> Reason;
reason({Error, _}) when is_atom(Error) -> Error;
reason(_) -> internal_error.

