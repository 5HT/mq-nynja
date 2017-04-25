var sessionId = parseInt(Math.random() * 100, 10);
var mqtt = new Paho.MQTT.Client("127.0.0.1", 8083, "N2O_" + sessionId);

mqtt.onConnectionLost = function (o) { console.log("connection lost: " + o.errorMessage); };
mqtt.onMessageArrived = function (m) { console.log('name: ' + m.destinationName + ', payload: ' + m.payloadString); };

var options = {
    timeout: 3,
    onSuccess: function () { console.log("Connected"); },
    onFailure: function (message) { console.log("Connection failed: " + message.errorMessage); }
};

var publish = function (payload, topic, qos) {
    var message = new Messaging.Message(payload);
    message.destinationName = topic;
    message.qos = qos;
    mqtt.send(message);
}

mqtt.connect(options);

