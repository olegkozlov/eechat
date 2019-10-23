-module(chat).

-export([start_link/0, init/0]).

start_link() ->
    {ok, spawn_link(?MODULE, init, [])}.

init() ->
    loop([]),
    ok.

loop(Users) -> 
    receive
        {From, join, Username} ->
            user_joined(From, Username, Users),
            loop([{From, Username}|Users]);

        {From, message} ->
            new_message(Users, From),
            loop(Users);

        _ ->
            loop(Users)
    end.

user_joined(_Pid, _Username, _Users) ->
    %broadcast(Pid, {joined, Username}),
    %broadcast(Users, {user_joined, Username}, Pid).
    ok.

new_message(_Users, _From) ->
    %broadcast(Users, {message, "Hello"}).
    ok.