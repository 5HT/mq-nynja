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

-module(emq_dashboard_user).

-include("emq_dashboard.hrl").

-include_lib("emqttd/include/emqttd.hrl").

-export([users/0, update/3, remover/1, add/3]).

-http_api({"users", users, []}).

-http_api({"update_user", update,  [{"user_name", binary},
                                    {"password",  binary},
                                    {"tags",      binary, <<"">>}]}).
-http_api({"remove_user", remover, [{"user_name", binary}]}).

-http_api({"add_user",    add,     [{"user_name", binary},
                                    {"password",  binary},
                                    {"tags",      binary, <<"">>}]}).

-define(EMPTY(S), (S == <<"">> orelse S == undefined)).

users() ->
    {ok, [row(Admin) || Admin <- ets:tab2list(mqtt_admin)]}.

row(#mqtt_admin{username = Username, tags = Tags}) ->
    [{name, Username}, {tag, Tags}].

add(Username, Password, _Tag) when ?EMPTY(Username) orelse ?EMPTY(Password) ->
    {ok, code({error, "Username or password undefined"})};
 
add(Username, Password, Tag) ->
    {ok, code(emq_dashboard_admin:add_user(Username, Password, Tag))}.

update(Username, Password, _Tag) when ?EMPTY(Username) orelse ?EMPTY(Password) ->
    {ok, code({error, "Username or password undefined"})};
 
update(Username, Password, Tag) ->
    {ok, code(emq_dashboard_admin:update_user(Username, Password, Tag))}.

remover(<<"admin">>) ->
    {ok, [{status, failure},{reason, list_to_binary("admin cannot be deleted")}]};

remover(Username) ->
    {ok, code(emq_dashboard_admin:remove_user(Username))}.
 
code(ok)              -> [{status, success}];
code({error, Reason}) -> [{status, failure}, {reason, list_to_binary(Reason)}].

