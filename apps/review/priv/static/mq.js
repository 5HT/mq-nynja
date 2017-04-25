var sessionId = parseInt(Math.random() * 100, 10);
var mqtt = new Paho.MQTT.Client("127.0.0.1", 8083, "N2O_" + sessionId);
var options = {
    timeout: 3,
    onSuccess: function () { console.log("N2O Connected");
                             mqtt.subscribe("n2o", subscribeOptions); },
    onFailure: function (message) { console.log("N2O Connection failed: " + message.errorMessage); }
};
var subscribeOptions = {
    qos: 0,  // QoS
    invocationContext: {foo: true},  // Passed to success / failure callback
    onSuccess: function () { console.log("N2O Subscribed"); },
    onFailure: function (message) { console.log("N2O Subscription failed: " + message.errorMessage); },
    timeout: 3
};

mqtt.onConnectionLost = function (o) { console.log("connection lost: " + o.errorMessage); };
mqtt.onMessageArrived = function (m) {
    var BERT = m.payloadBytes.buffer.slice(m.payloadBytes.byteOffset,
               m.payloadBytes.byteOffset+m.payloadBytes.length);
    try { erlang = dec(BERT);
          console.log(erlang);
          for (var i=0;i<$bert.protos.length;i++) {
             p = $bert.protos[i]; if (p.on(erlang, p.do).status == "ok") return; }
    } catch (e) { console.log(e); }
};


var ws = {
    send: function (payload) {
        var message = new Paho.MQTT.Message(payload);
        message.destinationName = "n2o";
        message.qos = 0;
        mqtt.send(message);
    }
};

mqtt.connect(options);
