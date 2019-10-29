import { Elm } from './elm/src/Main.elm'

let socket = new WebSocket("ws://localhost:8088/ws/")
socket.onopen = function (e) {
    console.log("Connected to the socket");
};

socket.onmessage = function(event) {
    app.ports.onResponse.send(event.data);
}

const app = Elm.Main.init({
    node: document.getElementById("root")
});

app.ports.toSocket.subscribe( function(data) {
    let json = JSON.stringify(data);
    socket.send(json);
});

let heartbeat = setInterval( function() {
    socket.send("ping");
}, 45000);