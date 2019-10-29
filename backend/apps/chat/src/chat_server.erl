-module(chat_server).

-export([start_link/0, init/0]).

start_link() ->
    Pid = spawn_link(?MODULE, init, []),
    register(chat_server, Pid),
    {ok, Pid}.

init() ->
    loop([]),
    ok.

loop(Users) -> 
    receive
        {From, join, Username} ->
            {ok, NewUsersList} = chat:user_joined(From, Username, Users),
            loop(NewUsersList);

        {From, message, Msg} ->
            chat:new_message(From, Users, Msg),
            loop(Users);

        {From, disconnected} ->
            {ok, NewUsersList} = chat:user_disconnected(From, Users),
            loop(NewUsersList);

        _ ->
            loop(Users)
    end.