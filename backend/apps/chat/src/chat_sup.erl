%%%-------------------------------------------------------------------
%% @doc chat top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(chat_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = {one_for_all, 10, 1},
    Chat = #{
        id => cs,
        start => {chat_server, start_link, []}
    },
    ChildSpecs = [Chat],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
