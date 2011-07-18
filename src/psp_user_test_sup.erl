-module(psp_user_test_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-export([start_child/2]).

-export([start/0]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, temporary, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%leech tactic to be supervised
start() ->
    supervisor:start_child(pubsubpoll_sup, {psp_user_test_sup, {psp_user_test_sup, start_link, []},
            permanent, 5000, worker, [psp_user_test_sup]}).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {simple_one_for_one, 5, 10}, [
        ?CHILD(psp_user_test, worker)
    ]} }.

start_child(Client, OnPoll) ->
    supervisor:start_child(?MODULE, [Client, OnPoll]).

