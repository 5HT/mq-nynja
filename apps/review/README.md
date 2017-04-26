MQTT Review Application
=======================

Here is example of working N2O Review Application on top of MQTT EMQ broker.

Run
---

After server is running you should enable `N2O over MQTT bridge` EMQ plugin
on plugin page http://127.0.0.1:18083/#/plugins and then open application
sample http://127.0.0.1:8000/spa/login.htm

HEART, n2o_session and AUTH
---------------------------

`N2O_start` and `n2o.js` is no longer used in MQTT version of N2O.
Instead `N2O_start` one should use `MQTT_start` and `mqtt.js` for session control replacement.
We traded `HEART` protocol and session facilities for bult-in MQTT features.
N2O authentication and authorization mechanism is also abandoned as MQTT
could provide AUTH features too.

```
<script src="//127.0.0.1:18083/assets/js/mqttws31.js"></script>
<script src="/spa/mq.js"></script>
```

Credits
-------
* Brought to you by 5HT and Andy

