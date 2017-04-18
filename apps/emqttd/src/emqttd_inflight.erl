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

%% @doc Inflight Window that wraps the gb_trees.

-module(emqttd_inflight).

-author("Feng Lee <feng@emqtt.io>").

-export([new/1, contain/2, lookup/2, insert/3, update/3, delete/2, values/1,
         to_list/1, size/1, max_size/1, is_full/1, is_empty/1, window/1]).

-type(inflight() :: {?MODULE, list()}).

-export_type([inflight/0]).

-spec(new(non_neg_integer()) -> inflight()).
new(MaxSize) when MaxSize >= 0 ->
    {?MODULE, [MaxSize, gb_trees:empty()]}.

-spec(contain(Key :: any(), inflight()) -> boolean()).
contain(Key, {?MODULE, [_MaxSize, Tree]}) ->
    gb_trees:is_defined(Key, Tree).

-spec(lookup(Key :: any(), inflight()) -> any()).
lookup(Key, {?MODULE, [_MaxSize, Tree]}) ->
    gb_trees:get(Key, Tree).

-spec(insert(Key :: any(), Value :: any(), inflight()) -> inflight()).
insert(Key, Value, {?MODULE, [MaxSize, Tree]}) ->
    {?MODULE, [MaxSize, gb_trees:insert(Key, Value, Tree)]}.

-spec(delete(Key :: any(), inflight()) -> inflight()).
delete(Key, {?MODULE, [MaxSize, Tree]}) ->
    {?MODULE, [MaxSize, gb_trees:delete(Key, Tree)]}.

-spec(update(Key :: any(), Val :: any(), inflight()) -> inflight()).
update(Key, Val, {?MODULE, [MaxSize, Tree]}) ->
    {?MODULE, [MaxSize, gb_trees:update(Key, Val, Tree)]}.

-spec(is_full(inflight()) -> boolean()).
is_full({?MODULE, [0, _Tree]}) ->
    false;
is_full({?MODULE, [MaxSize, Tree]}) ->
    MaxSize =< gb_trees:size(Tree).

-spec(is_empty(inflight()) -> boolean()).
is_empty({?MODULE, [_MaxSize, Tree]}) ->
    gb_trees:is_empty(Tree).

-spec(smallest(inflight()) -> {K :: any(), V :: any()}).
smallest({?MODULE, [_MaxSize, Tree]}) ->
    gb_trees:smallest(Tree).

-spec(largest(inflight()) -> {K :: any(), V :: any()}).
largest({?MODULE, [_MaxSize, Tree]}) ->
    gb_trees:largest(Tree).

-spec(values(inflight()) -> list()).
values({?MODULE, [_MaxSize, Tree]}) ->
    gb_trees:values(Tree).

-spec(to_list(inflight()) -> list({K :: any(), V :: any()})).
to_list({?MODULE, [_MaxSize, Tree]}) ->
    gb_trees:to_list(Tree).

-spec(window(inflight()) -> list()).
window(Inflight = {?MODULE, [_MaxSize, Tree]}) ->
    case gb_trees:is_empty(Tree) of
        true  -> [];
        false -> [Key || {Key, _Val} <- [smallest(Inflight), largest(Inflight)]]
    end.

-spec(size(inflight()) -> non_neg_integer()).
size({?MODULE, [_MaxSize, Tree]}) ->
    gb_trees:size(Tree).

-spec(max_size(inflight()) -> non_neg_integer()).
max_size({?MODULE, [MaxSize, _Tree]}) ->
    MaxSize.

