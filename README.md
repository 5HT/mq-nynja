MQTT Messaging Server
=====================

Features
--------

* EMQ version 2.1.1
* MAD compatible

Setup
-----

```
$ mad dep com rep
Configuration: [{emq_dashboard,
                    [{listeners_dash,
                         [{http,18083,[{acceptors,4},{max_clients,512}]}]}]},
                {emqttd,
                    [{listeners,
                         [{http,8083,[{acceptors,4},{max_clients,512}]}]},
                     {sysmon,
                         [{long_gc,false},
                          {long_schedule,240},
                          {large_heap,8000000},
                          {busy_port,false},
                          {busy_dist_port,true}]},
                     {session,
                         [{upgrade_qos,off},
                          {max_inflight,32},
                          {retry_interval,20},
                          {max_awaiting_rel,100},
                          {await_rel_timeout,20},
                          {enable_stats,off}]},
                     {queue,[]},
                     {allow_anonymous,true},
                     {protocol,
                         [{max_clientid_len,1024},{max_packet_size,64000}]},
                     {acl_file,"etc/acl.conf"},
                     {plugins_etc_dir,"etc/plugins/"},
                     {plugins_loaded_file,"etc/loaded_plugins"},
                     {pubsub,
                         [{pool_size,8},{by_clientid,true},{async,true}]}]},
                {emq_modules,
                    [{presence,[{qos,1}]},
                     {subscription,[{topic,"user/%c"},{qos,2}]},
                     {rewrite,
                         [{rule1,"x/# ^x/y/(.+)$ z/y/$1"},
                          {rule2,"y/+/z/# ^y/(.+)/z/(.+)$ y/z/$2"}]}]},
                {kvs,
                    [{dba,store_mnesia},
                     {schema,[kvs_user,kvs_acl,kvs_feed,kvs_subscription]}]},
                {n2o,[]}]
Applications:  [public_key,kernel,stdlib,gproc,lager_syslog,pbkdf2,gen_logger,
                mnesia,compiler,inets,crypto,syntax_tools,xmerl,esockd,
                goldrush,ssl,lager,mochiweb,kvs,mad,sh,syslog,emqttd]
Erlang/OTP 19 [erts-8.2] [source] [64-bit] [smp:4:4] [async-threads:10]
              [hipe] [kernel-poll:false] [dtrace]

Eshell V8.2  (abort with ^G)
1> 20:13:51.475 [info] Application lager started on node nonode@nohost
20:13:51.475 [info] Application mochiweb started on node nonode@nohost
20:13:51.476 [info] Application kvs started on node nonode@nohost
20:13:51.481 [info] Application mad started on node nonode@nohost
20:13:51.551 [info] Application sh started on node nonode@nohost
20:13:51.626 [info] Application syslog started on node nonode@nohost
starting emqttd on node 'nonode@nohost'
Nonexistent: []
emqttd ctl is starting...[ok]
emqttd hook is starting...[ok]
emqttd router is starting...[ok]
emqttd pubsub is starting...[ok]
emqttd stats is starting...[ok]
emqttd metrics is starting...[ok]
emqttd pooler is starting...[ok]
emqttd trace is starting...[ok]
emqttd client manager is starting...[ok]
emqttd session manager is starting...[ok]
emqttd session supervisor is starting...[ok]
emqttd wsclient supervisor is starting...[ok]
emqttd broker is starting...[ok]
emqttd alarm is starting...[ok]
emqttd mod supervisor is starting...[ok]
emqttd bridge supervisor is starting...[ok]
emqttd access control is starting...[ok]
emqttd system monitor is starting...[ok]
Plugins: [{mqtt_plugin,emq_auth_username,"2.1.1",
                       "Authentication with Username/Password",false},
          {mqtt_plugin,emq_dashboard,"2.1.1","EMQ Web Dashboard",false},
          {mqtt_plugin,emq_modules,"2.1.1","EMQ Modules",false}]
Names: [emq_dashboard]
Nonexistent: []
dashboard:http listen on 0.0.0.0:18083 with 4 acceptors.
20:13:53.803 [info] started Apps: [emq_dashboard]
20:13:53.803 [info] load plugin emq_dashboard successfully
20:13:53.803 [info] Application emq_dashboard started on node nonode@nohost
mqtt:ws listen on 0.0.0.0:8083 with 4 acceptors.
emqttd 2.1.1 is running now
20:13:53.804 [info] Application emqttd started on node nonode@nohost
```

Open http://127.0.0.1:18083/#/websocket with `admin:public` credentials, Press Connect, Subscribe, Sned and observe statistics http://127.0.0.1:18083/#/overview.

Adding Users
------------

```
> emq_auth_username:cli(["add","maxim","public"]).
```

Then enable `emq_auth_username` plugin in the dashboard http://127.0.0.1:18083/#/plugins
Later you can connect specifying `User Name:` and `Password:` credential at http://127.0.0.1:18083/#/websocket

Credits
-------

* Maxim Sokhatsky

OM A HUM


