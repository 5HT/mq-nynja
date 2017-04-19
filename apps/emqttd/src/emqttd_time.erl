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

-module(emqttd_time).

-author("Feng Lee <feng@emqtt.io>").

-export([seed/0, now_secs/0, now_secs/1, now_ms/0, now_ms/1, ts_from_ms/1]).

seed() ->
    rand:seed(exsplus, erlang:timestamp()).

now_ms() ->
    now_ms(os:timestamp()).

now_ms({MegaSecs, Secs, MicroSecs}) ->
    (MegaSecs * 1000000 + Secs) * 1000 + round(MicroSecs/1000).

now_secs() ->
    now_secs(os:timestamp()).

now_secs({MegaSecs, Secs, _MicroSecs}) ->
    MegaSecs * 1000000 + Secs.

ts_from_ms(Ms) ->
    {Ms div 1000000, Ms rem 1000000, 0}.

