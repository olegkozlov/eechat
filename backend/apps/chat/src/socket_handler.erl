-module(socket_handler).

-export([init/2, websocket_handle/2, websocket_init/1, websocket_info/2, terminate/3]).

init(Req, State) -> 
    {cowboy_websocket, Req, State}.

websocket_init(State) -> 
    {ok, State}.

websocket_handle({text, <<"ping">>}, State) ->
    {reply, pong, State};

websocket_handle({text, Json}, State) ->
    chat:proceed_request(Json),
    {ok, State};

websocket_handle(_Frame, State) -> 
    {ok, State}.

websocket_info({reply, Msg}, State) ->
    {reply, {text, Msg}, State}.

terminate(_Reason, _PartialReq, _State) -> 
    chat:proceed_request(disconnected, self()),
    ok.