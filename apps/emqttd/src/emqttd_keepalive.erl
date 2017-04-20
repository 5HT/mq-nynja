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

%% @doc Client Keepalive

-module(emqttd_keepalive).

-author("Feng Lee <feng@emqtt.io>").

-export([start/3, check/1, cancel/1]).

-record(keepalive, {statfun, statval, tsec, tmsg, tref, repeat = 0}).

-type(keepalive() :: #keepalive{}).

-export_type([keepalive/0]).

%% @doc Start a keepalive
-spec(start(fun(), integer(), any()) -> {ok, keepalive()} | {error, any()}).
start(_, 0, _) ->
    {ok, #keepalive{}};
start(StatFun, TimeoutSec, TimeoutMsg) ->
    case StatFun() of
        {ok, StatVal} ->
            {ok, #keepalive{statfun = StatFun, statval = StatVal,
                            tsec = TimeoutSec, tmsg = TimeoutMsg,
                            tref = timer(TimeoutSec, TimeoutMsg)}};
        {error, Error} ->
            {error, Error}
    end.

%% @doc Check keepalive, called when timeout.
-spec(check(keepalive()) -> {ok, keepalive()} | {error, any()}).
check(KeepAlive = #keepalive{statfun = StatFun, statval = LastVal, repeat = Repeat}) ->
    case StatFun() of
        {ok, NewVal} ->
            if NewVal =/= LastVal ->
                    {ok, resume(KeepAlive#keepalive{statval = NewVal, repeat = 0})};
                Repeat < 1 ->
                    {ok, resume(KeepAlive#keepalive{statval = NewVal, repeat = Repeat + 1})};
                true ->
                    {error, timeout}
            end;
        {error, Error} ->
            {error, Error}
    end.

resume(KeepAlive = #keepalive{tsec = TimeoutSec, tmsg = TimeoutMsg}) ->
    KeepAlive#keepalive{tref = timer(TimeoutSec, TimeoutMsg)}.

%% @doc Cancel Keepalive
-spec(cancel(keepalive()) -> ok).
cancel(#keepalive{tref = TRef}) when is_reference(TRef) ->
    catch erlang:cancel_timer(TRef), ok;
cancel(_) ->
    ok.

timer(Sec, Msg) ->
    erlang:send_after(timer:seconds(Sec), self(), Msg).

