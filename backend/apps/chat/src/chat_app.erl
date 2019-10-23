-module(chat_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->    

    Dispatch = cowboy_router:compile([
    {'_', [
        {"/ws/", hello_handler, []},
        {"/js/[...]", cowboy_static, {priv_dir, chat, "js/"}},
        {'_', cowboy_static, {priv_file, chat, "index.html"}}
    ]}
	]),
	{ok, _} = cowboy:start_clear(http, [{port, 8088}], #{
		env => #{dispatch => Dispatch}
	}),

    chat:start_link(),
    chat_sup:start_link().

stop(_State) ->
    ok.
