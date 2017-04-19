-module(emq_kvs_bridge).

-include_lib("emqttd/include/emqttd.hrl").
-include_lib("emqttd/include/emqttd_protocol.hrl").
-include_lib("emqttd/include/emqttd_internal.hrl").

-export([load/1, unload/0]).

%% Hooks functions
-export([on_client_connected/3, on_client_disconnected/3]).
-export([on_client_subscribe/3, on_client_subscribe_after/3, on_client_unsubscribe/3]).
-export([on_message_publish/2, on_message_delivered/3, on_message_acked/3]).

-record(struct, {lst=[]}).

%% Called when the plugin application start
load(Env) ->
    ekaf_init([Env]),
    emqttd:hook('client.connected', fun ?MODULE:on_client_connected/3, [Env]),
    emqttd:hook('client.disconnected', fun ?MODULE:on_client_disconnected/3, [Env]),
    emqttd:hook('client.subscribe', fun ?MODULE:on_client_subscribe/3, [Env]),
    emqttd:hook('client.subscribe.after', fun ?MODULE:on_client_subscribe_after/3, [Env]),
    emqttd:hook('client.unsubscribe', fun ?MODULE:on_client_unsubscribe/3, [Env]),
    emqttd:hook('message.publish', fun ?MODULE:on_message_publish/2, [Env]),
    emqttd:hook('message.delivered', fun ?MODULE:on_message_delivered/3, [Env]),
    emqttd:hook('message.acked', fun ?MODULE:on_message_acked/3, [Env]).

%%-----------client connect start-----------------------------------%%

on_client_connected(ConnAck, Client = #mqtt_client{client_id  = ClientId}, _Env) ->
    io:format("client ~s connected, connack: ~w~n", [ClientId, ConnAck]),

    Json = mochijson2:encode([
        {type, <<"connected">>},
        {client_id, ClientId},
        {cluster_node, node()},
        {ts, emqttd_time:now_to_secs()}
    ]),
    
    ekaf:produce_async_batched(<<"broker_message">>, list_to_binary(Json)),

    {ok, Client}.

%%-----------client connect end-------------------------------------%%



%%-----------client disconnect start---------------------------------%%

on_client_disconnected(Reason, ClientId, _Env) ->
    io:format("client ~s disconnected, reason: ~w~n", [ClientId, Reason]),

    Json = mochijson2:encode([
        {type, <<"disconnected">>},
        {client_id, ClientId},
        {reason, Reason},
        {cluster_node, node()},
        {ts, emqttd_time:now_to_secs()}
    ]),

    ekaf:produce_async_batched(<<"broker_message">>, list_to_binary(Json)),

    ok.

%%-----------client disconnect end-----------------------------------%%



%%-----------client subscribed start---------------------------------------%%

%% should retain TopicTable
on_client_subscribe(ClientId, TopicTable, _Env) ->
    io:format("client ~s will subscribe ~p~n", [ClientId, TopicTable]),
    {ok, TopicTable}.
   
on_client_subscribe_after(ClientId, TopicTable, _Env) ->
    io:format("client ~s subscribed ~p~n", [ClientId, TopicTable]),
    
    case TopicTable of
        [_|_] -> 
            %% If TopicTable list is not empty
            Key = proplists:get_keys(TopicTable),
            %% build json to send using ClientId
            Json = mochijson2:encode([
                {type, <<"subscribed">>},
                {client_id, ClientId},
                {topic, lists:last(Key)},
                {cluster_node, node()},
                {ts, emqttd_time:now_to_secs()}
            ]),
            ekaf:produce_async_batched(<<"broker_message">>, list_to_binary(Json));
        _ -> 
            %% If TopicTable is empty
            io:format("empty topic ~n")
    end,

    {ok, TopicTable}.

%%-----------client subscribed end----------------------------------------%%



%%-----------client unsubscribed start----------------------------------------%%

on_client_unsubscribe(ClientId, Topics, _Env) ->
    io:format("client ~s unsubscribe ~p~n", [ClientId, Topics]),

    % build json to send using ClientId
    Json = mochijson2:encode([
        {type, <<"unsubscribed">>},
        {client_id, ClientId},
        {topic, lists:last(Topics)},
        {cluster_node, node()},
        {ts, emqttd_time:now_to_secs()}
    ]),
    
    ekaf:produce_async_batched(<<"broker_message">>, list_to_binary(Json)),
    
    {ok, Topics}.

%%-----------client unsubscribed end----------------------------------------%%



%%-----------message publish start--------------------------------------%%

%% transform message and return
on_message_publish(Message = #mqtt_message{topic = <<"$SYS/", _/binary>>}, _Env) ->
    {ok, Message};

on_message_publish(Message, _Env) ->
    io:format("publish ~s~n", [emqttd_message:format(Message)]),   

    From = Message#mqtt_message.from,
%    Sender =  Message#mqtt_message.sender,
    Topic = Message#mqtt_message.topic,
    Payload = Message#mqtt_message.payload, 
    QoS = Message#mqtt_message.qos,
    Timestamp = Message#mqtt_message.timestamp,

    Json = mochijson2:encode([
        {type, <<"published">>},
        {client_id, From},
        {topic, Topic},
        {payload, Payload},
        {qos, QoS},
        {cluster_node, node()},
        {ts, emqttd_time:now_to_secs(Timestamp)}
    ]),

    ekaf:produce_async_batched(<<"broker_message">>, list_to_binary(Json)),

    {ok, Message}.

%%-----------message delivered start--------------------------------------%%
on_message_delivered(ClientId, Message, _Env) ->
    io:format("delivered to client ~s: ~s~n", [ClientId, emqttd_message:format(Message)]),

    From = Message#mqtt_message.from,
 %   Sender =  Message#mqtt_message.sender,
    Topic = Message#mqtt_message.topic,
    Payload = Message#mqtt_message.payload, 
    QoS = Message#mqtt_message.qos,
    Timestamp = Message#mqtt_message.timestamp,

    Json = mochijson2:encode([
        {type, <<"delivered">>},
        {client_id, ClientId},
        {from, From},
        {topic, Topic},
        {payload, Payload},
        {qos, QoS},
        {cluster_node, node()},
        {ts, emqttd_time:now_to_secs(Timestamp)}
    ]),

    ekaf:produce_async_batched(<<"broker_message">>, list_to_binary(Json)),

    {ok, Message}.
%%-----------message delivered end----------------------------------------%%

%%-----------acknowledgement publish start----------------------------%%
on_message_acked(ClientId, Message, _Env) ->
    io:format("client ~s acked: ~s~n", [ClientId, emqttd_message:format(Message)]),   

    From = Message#mqtt_message.from,
%    Sender =  Message#mqtt_message.sender,
    Topic = Message#mqtt_message.topic,
    Payload = Message#mqtt_message.payload, 
    QoS = Message#mqtt_message.qos,
    Timestamp = Message#mqtt_message.timestamp,

    Json = mochijson2:encode([
        {type, <<"acked">>},
        {client_id, ClientId},
        {from, From},
        {topic, Topic},
        {payload, Payload},
        {qos, QoS},
        {cluster_node, node()},
        {ts, emqttd_time:now_to_secs(Timestamp)}
    ]),

    ekaf:produce_async_batched(<<"broker_message">>, list_to_binary(Json)),
    {ok, Message}.

%% ===================================================================
%% ekaf_init
%% ===================================================================

ekaf_init(_Env) ->
    %% Get parameters
    {ok, Kafka} = application:get_env(emq_persistence, kafka),
    BootstrapBroker = proplists:get_value(bootstrap_broker, Kafka),
    PartitionStrategy= proplists:get_value(partition_strategy, Kafka),
    %% Set partition strategy, like application:set_env(ekaf, ekaf_partition_strategy, strict_round_robin),
    application:set_env(ekaf, ekaf_partition_strategy, PartitionStrategy),
    %% Set broker url and port, like application:set_env(ekaf, ekaf_bootstrap_broker, {"127.0.0.1", 9092}),
    application:set_env(ekaf, ekaf_bootstrap_broker, BootstrapBroker),
    %% Set topic
    application:set_env(ekaf, ekaf_bootstrap_topics, <<"broker_message">>),

    io:format("Init ekaf with ~p~n", [BootstrapBroker]).


%% Called when the plugin application stop
unload() ->
    emqttd:unhook('client.connected', fun ?MODULE:on_client_connected/3),
    emqttd:unhook('client.disconnected', fun ?MODULE:on_client_disconnected/3),
    emqttd:unhook('client.subscribe', fun ?MODULE:on_client_subscribe/3),
    emqttd:unhook('client.subscribe.after', fun ?MODULE:on_client_subscribe_after/3),
    emqttd:unhook('client.unsubscribe', fun ?MODULE:on_client_unsubscribe/3),
    emqttd:unhook('message.publish', fun ?MODULE:on_message_publish/2),
    emqttd:unhook('message.acked', fun ?MODULE:on_message_acked/3),
    emqttd:unhook('message.delivered', fun ?MODULE:on_message_delivered/3).

