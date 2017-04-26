MQTT Review Application
=======================

Here is example of working N2O Review Application on top of MQTT EMQ broker.

Run
---

After server is running you should enable `N2O over MQTT bridge` EMQ plugin
on plugin page http://127.0.0.1:18083/#/plugins and then open application
sample http://127.0.0.1:8000/spa/login.htm

HEART, n2o_session, AUTH and MQ
-------------------------------

`N2O_start` and `n2o.js` is no longer used in MQTT version of N2O.
Instead `N2O_start` one should use `MQTT_start` and `mqtt.js` for session control replacement.
We traded `HEART` protocol and session facilities for bult-in MQTT features.
N2O authentication and authorization mechanism is also abandoned as MQTT
could provide AUTH features too. Obviously `wf:reg` and `wf:send` API
is also abandoned as we can use `emqttd` API directly and `{deliver,_}` protocol of
`ws_client` gen_server. 

What is added to N2O?
---------------------

The one bad things about MQTT version is that we need to store now
both MQTT and BERT formatters on client.

```
<script src="//127.0.0.1:18083/assets/js/mqttws31.js"></script>
<script src="/spa/mq.js"></script>
```

Which layers are removed from MQTT version of N2O?
--------------------------------------------------

* n2o_session
* n2o_stream
* n2o_heart
* n2o_mq
* n2o_query
* N2O.js
* ranch
* cowboy

Key Things N2O is relying on
----------------------------

N2O is working entirely in context of `ws_client` sessions of EMQ, just
as it is working on top of `cowboy`. No other additional gen_servers are being
introduced.

The only official transparent way with zero abstractions is to use EMQ hooks
mechanism. For N2O we need to implement only two cases `client.subscribe` and
`message.delivered`. On client subscribe we deliver all persistent information
that are ready for the client. On Message delivered we to unpacking any N2O
BERT protocol message inside MQTT session.

The single point of entrance is the `event(init)` message handler.
It can only be reached by calling `n2o_nitrogen` with `{init,<<>>}` protocol message.
However N2O MQTT is a protocol federation so we need to handle not only N2O messages,
but also KVS, ROSTER, BPE, REST protocols.
Thus after initialization during `client.subscribe`  we call `n2o_proto:info` — 
the entire N2O protocol chain recursor inside `message.delivered` hook. 
This is a best place to put the federation relay for N2O modules.

Credits
-------
* Brought to you by 5HT and Andy

