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

-module(emqttd_sm).
-compile({parse_transform, lager_transform}).

-author("Feng Lee <feng@emqtt.io>").

-behaviour(gen_server2).

-include("emqttd.hrl").

-include("emqttd_internal.hrl").

%% Mnesia Callbacks
-export([mnesia/1]).

-boot_mnesia({mnesia, [boot]}).
-copy_mnesia({mnesia, [copy]}).

%% API Function Exports
-export([start_link/2]).

-export([start_session/2, lookup_session/1, register_session/3, unregister_session/1]).

-export([dispatch/3]).

-export([local_sessions/0]).

%% gen_server Function Exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% gen_server2 priorities
-export([prioritise_call/4, prioritise_cast/3, prioritise_info/3]).

-record(state, {pool, id, monitors}).

-define(POOL, ?MODULE).

-define(TIMEOUT, 120000).

-define(LOG(Level, Format, Args, Session),
            lager:Level("SM(~s): " ++ Format, [Session#mqtt_session.client_id | Args])).

%%--------------------------------------------------------------------
%% Mnesia callbacks
%%--------------------------------------------------------------------

mnesia(boot) ->
    %% Global Session Table
    ok = emqttd_mnesia:create_table(mqtt_session, [
                {type, set},
                {ram_copies, [node()]},
                {record_name, mqtt_session},
                {attributes, record_info(fields, mqtt_session)}]);

mnesia(copy) ->
    ok = emqttd_mnesia:copy_table(mqtt_session).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

%% @doc Start a session manager
-spec(start_link(atom(), pos_integer()) -> {ok, pid()} | ignore | {error, any()}).
start_link(Pool, Id) ->
    gen_server2:start_link({local, ?PROC_NAME(?MODULE, Id)}, ?MODULE, [Pool, Id], []).

%% @doc Start a session
-spec(start_session(boolean(), {binary(), binary() | undefined}) -> {ok, pid(), boolean()} | {error, any()}).
start_session(CleanSess, {ClientId, Username}) ->
    SM = gproc_pool:pick_worker(?POOL, ClientId),
    call(SM, {start_session, CleanSess, {ClientId, Username}, self()}).

%% @doc Lookup a Session
-spec(lookup_session(binary()) -> mqtt_session() | undefined).
lookup_session(ClientId) ->
    case mnesia:dirty_read(mqtt_session, ClientId) of
        [Session] -> Session;
        []        -> undefined
    end.

%% @doc Register a session with info.
-spec(register_session(binary(), boolean(), [tuple()]) -> true).
register_session(ClientId, CleanSess, Properties) ->
    ets:insert(mqtt_local_session, {ClientId, self(), CleanSess, Properties}).

%% @doc Unregister a session.
-spec(unregister_session(binary()) -> true).
unregister_session(ClientId) ->
    ets:delete(mqtt_local_session, ClientId).

dispatch(ClientId, Topic, Msg) ->
    try ets:lookup_element(mqtt_local_session, ClientId, 2) of
        Pid -> Pid ! {dispatch, Topic, Msg}
    catch
        error:badarg -> ok %%FIXME Later.
    end.

call(SM, Req) ->
    gen_server2:call(SM, Req, ?TIMEOUT). %%infinity).

%% @doc for debug.
local_sessions() ->
    ets:tab2list(mqtt_local_session).

%%--------------------------------------------------------------------
%% gen_server Callbacks
%%--------------------------------------------------------------------

init([Pool, Id]) ->
    ?GPROC_POOL(join, Pool, Id),
    {ok, #state{pool = Pool, id = Id, monitors = dict:new()}}.

prioritise_call(_Msg, _From, _Len, _State) ->
    1.

prioritise_cast(_Msg, _Len, _State) ->
    0.

prioritise_info(_Msg, _Len, _State) ->
    2.

%% Persistent Session
handle_call({start_session, false, {ClientId, Username}, ClientPid}, _From, State) ->
    case lookup_session(ClientId) of
        undefined ->
            %% Create session locally
            create_session({false, {ClientId, Username}, ClientPid}, State);
        Session ->
            case resume_session(Session, ClientPid) of
                {ok, SessPid} ->
                    {reply, {ok, SessPid, true}, State};
                {error, Erorr} ->
                    {reply, {error, Erorr}, State}
             end
    end;

%% Transient Session
handle_call({start_session, true, {ClientId, Username}, ClientPid}, _From, State) ->
    Client = {true, {ClientId, Username}, ClientPid},
    case lookup_session(ClientId) of
        undefined ->
            create_session(Client, State);
        Session ->
            case destroy_session(Session) of
                ok ->
                    create_session(Client, State);
                {error, Error} ->
                    {reply, {error, Error}, State}
            end
    end;

handle_call(Req, _From, State) ->
    ?UNEXPECTED_REQ(Req, State).

handle_cast(Msg, State) ->
    ?UNEXPECTED_MSG(Msg, State).

handle_info({'DOWN', MRef, process, DownPid, _Reason}, State) ->
    case dict:find(MRef, State#state.monitors) of
        {ok, ClientId} ->
            mnesia:transaction(fun() ->
                case mnesia:wread({mqtt_session, ClientId}) of
                    [] ->
                        ok;
                    [Sess = #mqtt_session{sess_pid = DownPid}] ->
                        emqttd_stats:del_session_stats(ClientId),
                        mnesia:delete_object(mqtt_session, Sess, write);
                    [_Sess] ->
                        ok
                    end
                end),
            {noreply, erase_monitor(MRef, State), hibernate};
        error ->
            lager:error("MRef of session ~p not found", [DownPid]),
            {noreply, State}
    end;

handle_info(Info, State) ->
    ?UNEXPECTED_INFO(Info, State).

terminate(_Reason, #state{pool = Pool, id = Id}) ->
    ?GPROC_POOL(leave, Pool, Id).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

%% Create Session Locally
create_session({CleanSess, {ClientId, Username}, ClientPid}, State) ->
    case create_session(CleanSess, {ClientId, Username}, ClientPid) of
        {ok, SessPid} ->
            {reply, {ok, SessPid, false},
                monitor_session(ClientId, SessPid, State)};
        {error, Error} ->
            {reply, {error, Error}, State}
    end.

create_session(CleanSess, {ClientId, Username}, ClientPid) ->
    case emqttd_session_sup:start_session(CleanSess, {ClientId, Username}, ClientPid) of
        {ok, SessPid} ->
            Session = #mqtt_session{client_id = ClientId, sess_pid = SessPid, clean_sess = CleanSess},
            case insert_session(Session) of
                {aborted, {conflict, ConflictPid}} ->
                    %% Conflict with othe node?
                    lager:error("SM(~s): Conflict with ~p", [ClientId, ConflictPid]),
                    {error, mnesia_conflict};
                {atomic, ok} ->
                    {ok, SessPid}
            end;
        {error, Error} ->
            {error, Error}
    end.

insert_session(Session = #mqtt_session{client_id = ClientId}) ->
    mnesia:transaction(
      fun() ->
        case mnesia:wread({mqtt_session, ClientId}) of
            [] ->
                mnesia:write(mqtt_session, Session, write);
            [#mqtt_session{sess_pid = SessPid}] ->
                mnesia:abort({conflict, SessPid})
        end
      end).

%% Local node
resume_session(Session = #mqtt_session{client_id = ClientId, sess_pid = SessPid}, ClientPid)
    when node(SessPid) =:= node() ->

    case is_process_alive(SessPid) of
        true ->
            emqttd_session:resume(SessPid, ClientId, ClientPid),
            {ok, SessPid};
        false ->
            ?LOG(error, "Cannot resume ~p which seems already dead!", [SessPid], Session),
            {error, session_died}
    end;

%% Remote node
resume_session(Session = #mqtt_session{client_id = ClientId, sess_pid = SessPid}, ClientPid) ->
    Node = node(SessPid),
    case rpc:call(Node, emqttd_session, resume, [SessPid, ClientId, ClientPid]) of
        ok ->
            {ok, SessPid};
        {badrpc, nodedown} ->
            ?LOG(error, "Session died for node '~s' down", [Node], Session),
            remove_session(Session),
            {error, session_nodedown};
        {badrpc, Reason} ->
            ?LOG(error, "Failed to resume from node ~s for ~p", [Node, Reason], Session),
            {error, Reason}
    end.

%% Local node
destroy_session(Session = #mqtt_session{client_id = ClientId, sess_pid  = SessPid})
    when node(SessPid) =:= node() ->
    emqttd_session:destroy(SessPid, ClientId),
    remove_session(Session);

%% Remote node
destroy_session(Session = #mqtt_session{client_id = ClientId,
                                        sess_pid  = SessPid}) ->
    Node = node(SessPid),
    case rpc:call(Node, emqttd_session, destroy, [SessPid, ClientId]) of
        ok ->
            remove_session(Session);
        {badrpc, nodedown} ->
            ?LOG(error, "Node '~s' down", [Node], Session),
            remove_session(Session); 
        {badrpc, Reason} ->
            ?LOG(error, "Failed to destory ~p on remote node ~p for ~s",
                 [SessPid, Node, Reason], Session),
            {error, Reason}
     end.

remove_session(Session) ->
    case mnesia:transaction(fun mnesia:delete_object/1, [Session]) of
        {atomic, ok}     -> ok;
        {aborted, Error} -> {error, Error}
    end.

monitor_session(ClientId, SessPid, State = #state{monitors = Monitors}) ->
    MRef = erlang:monitor(process, SessPid),
    State#state{monitors = dict:store(MRef, ClientId, Monitors)}.

erase_monitor(MRef, State = #state{monitors = Monitors}) ->
    State#state{monitors = dict:erase(MRef, Monitors)}.

