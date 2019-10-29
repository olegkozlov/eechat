-module(chat).

-export([proceed_request/1,proceed_request/2, user_joined/3, user_disconnected/2, new_message/3]).

proceed_request(Json) ->
    Map = jsx:decode(Json, [return_maps]),
    {ok, Action} = get_action(Map),
    case Action of
        <<"join">> ->
            {ok, Username} = get_username(Map),
            chat_server ! {self(), join, Username};
        <<"message">> ->
            {ok, Msg} = get_message(Map),
            chat_server ! {self(), message, Msg};
        _ ->
            io:format("unknown action~n")
    end,
    ok.

proceed_request(disconnected, Pid) ->
    chat_server ! {Pid, disconnected}.

user_joined(Pid, Username, Users) ->
    Msg = jsx:encode(#{
        <<"event">> => <<"Joined">>
    }),
    broadcast(Pid, {reply, Msg}),
    user_joined_notify_others(Users, Username),    
    users_list_updated([{Pid, Username}|Users]).

user_joined_notify_others(UsersList, Username) ->
    Msg = jsx:encode(#{
        <<"event">> => <<"NewUserJoined">>, 
        <<"payload">> => #{
            <<"username">> => Username
        }
    }),
    broadcast(UsersList, {reply, Msg}),
    {ok, UsersList}.

users_list_updated(UsersList) -> 
    Msg = jsx:encode(#{
        <<"event">> => <<"UsersListState">>, 
        <<"payload">> => #{
            <<"users">> => get_usernames(UsersList)
        }
    }),
    broadcast(UsersList, {reply, Msg}),
    {ok, UsersList}.

user_disconnected(Pid, Users) ->
    {_, Username} = get_username_by_pid(Pid, Users),
    UsersList = remove_user(Pid, Users),
    Msg = jsx:encode(#{
        <<"event">> => <<"UserDisconnected">>, 
        <<"payload">> => #{
            <<"username">> => Username
        }
    }),
    broadcast(UsersList, {reply, Msg}),
    users_list_updated(UsersList).

new_message(Pid, Users, Message) ->
    {_, Username} = get_username_by_pid(Pid, Users),
    Msg = jsx:encode(#{
        <<"event">> => <<"NewMessage">>, 
        <<"payload">> => #{
            <<"message">> => #{
                <<"msgType">> => <<"UserMessage">>,
                <<"from">> => Username,
                <<"message">> => Message
            }
        }
    }),
    broadcast(Users, {reply, Msg}),
    ok.

%% Values from map
get_action(Map) ->
    #{<<"action">> := Action} = Map,
    {ok, Action}.

get_username(Map) ->
    #{<<"nickname">> := Username} = Map,
    {ok, Username}.

get_message(Map) ->
    #{<<"message">> := Msg} = Map,
    {ok, Msg}.

%% Helpers

%% Return usernme by Pid
get_username_by_pid(Pid, Users) ->
    lists:keyfind(Pid, 1, Users).

%% Return list of usernames
get_usernames(Users) ->
    lists:map(fun({_,Username}) -> Username end, Users).

%% Remove single user from list
remove_user(Pid, Users) ->
    lists:filter(fun({PidInLIst, _}) -> PidInLIst =/= Pid end, Users).

%% Broadcasting
%% Broadcast to the single user
broadcast(Pid, Msg) when is_pid(Pid) ->
    Pid ! Msg;

%% Broadcast to all users
broadcast([], _Msg) -> 
    ok;

broadcast([{Pid,_Name}|T], Msg) ->
	Pid ! Msg,
	broadcast(T, Msg).

%% Send message to all users, except one which provided by third parameter.
broadcast([], _Msg, _Self) -> 
    ok;

broadcast([{Self,_Name}|T], Msg, Self) ->
	broadcast(T, Msg, Self) ;

broadcast([{Pid, _Name}|T], Msg, Self) ->
	Pid ! Msg,
	broadcast(T, Msg, Self).