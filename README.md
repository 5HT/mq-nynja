N2O over MQTT using EMQ Broker
==============================

Features
--------

* EMQ Version 2.1.1 MQTT Protocol 5
* MAD Compatible
* N2O Bridge as EMQ Plugin
* N2O Review Application

Setup
-----

```
$ curl -fsSL https://raw.github.com/synrc/mad/master/mad > mad \
              && chmod +x mad \
              && sudo cp mad /usr/local/bin
$ mad dep com rep
Configuration: [{n2o,
                    [{port,8000},
                     {app,review},
                     {pickler,n2o_secret},
                     {formatter,bert},
                     {log_modules,config},
                     {log_level,config}]},
                {emq_dashboard,
                    [{listeners_dash,
                         [{http,18083,[{acceptors,4},{max_clients,512}]}]}]},
                {emq_modules,
                    [{modules,
                         [{emq_mod_presence,[{qos,1}]},
                          {emq_mod_subscription,[{<<"%u/%c/#">>,2}]},
                          {emq_mod_rewrite,
                              [{rewrite,"x/#","^x/y/(.+)$","z/y/$1"},
                               {rewrite,"y/+/z/#","^y/(.+)/z/(.+)$",
                                   "y/z/$2"}]}]}]},
                {emqttd,
                    [{listeners,
                         [{http,8083,[{acceptors,4},{max_clients,512}]},
                          {tcp,1883,[{acceptors,4},{max_clients,512}]}]},
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
                {kvs,
                    [{dba,store_mnesia},
                     {schema,[kvs_user,kvs_acl,kvs_feed,kvs_subscription]}]}]
Applications:  [kernel,stdlib,gproc,lager_syslog,pbkdf2,asn1,fs,ranch,mnesia,
                compiler,inets,crypto,syntax_tools,xmerl,gen_logger,esockd,
                cowlib,goldrush,public_key,lager,ssl,cowboy,mochiweb,emqttd,
                erlydtl,kvs,mad,emqttc,nitro,rest,sh,syslog,review]
Erlang/OTP 19 [erts-8.3] [source] [64-bit] [smp:4:4]
              [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Eshell V8.3  (abort with ^G)
starting emqttd on node 'nonode@nohost'
Nonexistent: []
Plugins: [{mqtt_plugin,emq_auth_username,"2.1.1",
                       "Authentication with Username/Password",false},
          {mqtt_plugin,emq_dashboard,"2.1.1","EMQ Web Dashboard",false},
          {mqtt_plugin,emq_modules,"2.1.1","EMQ Modules",false},
          {mqtt_plugin,n2o,"4.5-mqtt","N2O Server",false}]
Names: [emq_dashboard,n2o]
dashboard:http listen on 0.0.0.0:18083 with 4 acceptors.
Async Start Attempt {handler,"timer",n2o,system,n2o,[],[]}
Proc Init: init
mqtt:ws listen on 0.0.0.0:8083 with 4 acceptors.
mqtt:tcp listen on 0.0.0.0:1883 with 4 acceptors.
emqttd 2.1.1 is running now
>
```

Open http://127.0.0.1:18083/#/websocket with `admin:public` credentials, Press Connect, Subscribe, Sned and observe statistics http://127.0.0.1:18083/#/overview.

Create Authorized User
----------------------

```
> emq_auth_username:cli(["add","maxim","public"]).
```

Then enable `emq_auth_username` plugin in the dashboard http://127.0.0.1:18083/#/plugins
Later you can connect specifying `User Name:` and `Password:` credentials
at http://127.0.0.1:18083/#/websocket

Creating Single File Bundle
---------------------------

```
$ mad release emqttd
$ ./emqttd rep
```

Control Panel
-------------

```
> emqttd_ctl:run(["plugins","list"]).
Plugin(emq_auth_username, version=2.1.1, description=Authentication, active=false)
Plugin(emq_dashboard, version=2.1.1, description=EMQ Web Dashboard, active=true)
Plugin(emq_modules, version=2.1.1, description=EMQ Modules, active=true)
Plugin(emq_persistence, version=1.1.2, description=Synrc KVS for MQTT, active=true)
ok

> emqttd_ctl:run(["clients","list"]).
Client(C_1492632081463, clean_sess=true, username=5HT,
       peername=127.0.0.1:58225, connected_at=1492632082)
ok

> emqttd_ctl:run(["help"]).
```

MQTT Erlang Client
------------------

```
$ mad com
==> "/Users/maxim/depot/voxoz/emqttc/examples/gen_server"
Compiling /src/gen_server_example.erl
Writing /ebin/gen_server_example.app
OK
bash-3.2$ ./run
Erlang/OTP 19 [erts-8.2] [source] [64-bit] [smp:4:4]
              [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Eshell V8.2  (abort with ^G)
1> [info] [Client <0.58.0>]: connecting to 127.0.0.1:1883
[info] [Client <0.58.0>] connected with 127.0.0.1:1883
[info] [Client <0.58.0>] RECV: CONNACK_ACCEPT
Client <0.58.0> is connected
[warning] [simpleClient@127.0.0.1:64618] resubscribe [{<<"TopicA">>,1}]
Message from TopicA: <<"hello...1">>
Message from TopicB: <<"hello...1">>
```

Credits
-------

* Maxim Sokhatsky

OM A HUM
