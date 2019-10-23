import { Elm } from './elm/src/Main.elm'

//let socket = new WebSocket("ws://localhost:8088/ws/")
//socket.onopen = function (e) {
//    console.log("Connected to the socket");
//};

const app = Elm.Main.init({
    node: document.getElementById("root")
});

app.ports.toSocket.subscribe( function(data) {
    console.log(data);
    app.ports.onResponse.send(
        JSON.stringify({ event : "SelfJoined" })
    );
});

app.ports.onResponse.send(
    JSON.stringify({ 
        event : "ChatState", 
        payload : { 
            users : ["Andy", "Mary", "Wolfgan", "Pol", "Kianu", "Elisabeth", "Marianna"]
        } 
    })
);

app.ports.onResponse.send(
    JSON.stringify({ 
        event : "NewMessage", 
        payload : { 
            message : { 
                msgType : "UserMessage",
                from : "Andy",
                message : "This is incoming user message"
            }
        } 
    })
);