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

-module(emq_mod_rewrite).
-compile({parse_transform, lager_transform}).

-behaviour(emqttd_gen_mod).

-author("Feng Lee <feng@emqtt.io>").

-include_lib("emqttd/include/emqttd.hrl").

-export([load/1, unload/1]).

-export([rewrite_subscribe/4, rewrite_unsubscribe/4, rewrite_publish/2]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

load(Rules0) ->
    Rules = compile(Rules0),
    emqttd:hook('client.subscribe',  fun ?MODULE:rewrite_subscribe/4, [Rules]),
    emqttd:hook('client.unsubscribe',fun ?MODULE:rewrite_unsubscribe/4, [Rules]),
    emqttd:hook('message.publish',   fun ?MODULE:rewrite_publish/2, [Rules]).

rewrite_subscribe(_ClientId, _Username, TopicTable, Rules) ->
    lager:info("Rewrite subscribe: ~p", [TopicTable]),
    {ok, [{match_rule(Topic, Rules), Opts} || {Topic, Opts} <- TopicTable]}.

rewrite_unsubscribe(_ClientId, _Username, TopicTable, Rules) ->
    lager:info("Rewrite unsubscribe: ~p", [TopicTable]),
    {ok, [{match_rule(Topic, Rules), Opts} || {Topic, Opts} <- TopicTable]}.

rewrite_publish(Message=#mqtt_message{topic = Topic}, Rules) ->
    %%TODO: this will not work if the client is always online.
    RewriteTopic =
    case get({rewrite, Topic}) of
        undefined ->
            DestTopic = match_rule(Topic, Rules),
            put({rewrite, Topic}, DestTopic), DestTopic;
        DestTopic ->
            DestTopic
        end,
    {ok, Message#mqtt_message{topic = RewriteTopic}}.

unload(_) ->
    emqttd:unhook('client.subscribe',  fun ?MODULE:rewrite_subscribe/4),
    emqttd:unhook('client.unsubscribe',fun ?MODULE:rewrite_unsubscribe/4),
    emqttd:unhook('message.publish',   fun ?MODULE:rewrite_publish/2).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

match_rule(Topic, []) ->
    Topic;

match_rule(Topic, [{rewrite, Filter, MP, Dest} | Rules]) ->
    case emqttd_topic:match(Topic, Filter) of
        true  -> match_regx(Topic, MP, Dest);
        false -> match_rule(Topic, Rules)
    end.

match_regx(Topic, MP, Dest) ->
    case re:run(Topic, MP, [{capture, all_but_first, list}]) of
        {match, Captured} ->
            Vars = lists:zip(["\\$" ++ integer_to_list(I)
                                || I <- lists:seq(1, length(Captured))], Captured),
            iolist_to_binary(lists:foldl(
                    fun({Var, Val}, Acc) ->
                        re:replace(Acc, Var, Val, [global])
                    end, Dest, Vars));
        nomatch ->
            Topic
    end.

compile(Rules) ->
    lists:map(fun({rewrite, Topic, Re, Dest}) ->
                  {ok, MP} = re:compile(Re),
                  {rewrite, Topic, MP, Dest}
              end, Rules).

