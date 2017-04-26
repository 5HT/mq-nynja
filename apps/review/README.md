MQTT Review Application
=======================

Here is example of working N2O Review Application on top of MQTT EMQ broker.

Motivation
----------

Prepare N2O protocol federation to commercial use on top of MQTT protocol.
Minimize and remove all features, duplicated by MQTT pubsub broker.
Provide EMQ extension that immediately introduce N2O protocol and
application to connected MQTT devices. Create single Erlang eco-system
for Enterprise Protocol Federation and establish
solid CORBA-, WS-, XMPP-replacement, ready for high-speed,
low-latency IoT applications.

Run
---

After server is running you should enable `N2O over MQTT bridge` EMQ plugin
on plugin page http://127.0.0.1:18083/#/plugins and then open application
sample http://127.0.0.1:8000/spa/login.htm

PING, SESSION, AUTH and MQ layers of N2O
----------------------------------------

`N2O_start` and `n2o.js` is no longer used in MQTT version of N2O.
Instead `N2O_start` one should use `MQTT_start` and `mqtt.js` for session control replacement.
We traded `HEART` protocol and session facilities for bult-in MQTT features.
N2O authentication and authorization mechanism is also abandoned as MQTT
could provide AUTH features too. Obviously `wf:reg` and `wf:send` API
is also abandoned as we can use `emqttd` API directly and `{deliver,_}` protocol of
`ws_client` gen_server. 

What is added to N2O?
---------------------

The one bad thing about MQTT version is that we need to store now
both MQTT and BERT formatters on client.

```
<script src="//127.0.0.1:18083/assets/js/mqttws31.js"></script>
<script src="/spa/mq.js"></script>
```

Also IBM version of MQTT JavaScript library is far beyond the
speed and byte magic of `bert.js` library provided by N2O.
We packed BERT encoding inside MSS/MTU and so we see
WS31 replacement as desired.

Which layers are removed from MQTT version of N2O?
--------------------------------------------------

This is a good part.

* n2o_session — no Browser, so no Cookies are needed
* n2o_stream — no XHR fallback needed
* n2o_heart — no PING protocol needed
* n2o_mq — `syn` and `gproc` are no longer neede
* n2o_query — no Query Router 
* N2O.js — no pinger
* ranch — `esockd` instead
* cowboy — `mochiweb` for WebSockets inside EMQ

NOTE: WebSockets are not the most capacitive transport, the
MQTT-SN extension is able to work on UDP streams.
MQTT can work only over TCP for raw speed.

Key Things N2O is relying on
----------------------------

N2O is working entirely in context of `ws_client` processes of EMQ, just
as it is working on top of `ranch` processes of `cowboy`.
No additional `gen_server` is being introduced.

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
* Brought to you by 5HT and M2K

