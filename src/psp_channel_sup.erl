-module(psp_channel_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-export([start_child/3]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, temporary, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {simple_one_for_one, 5, 10}, [
        ?CHILD(psp_channel, worker)
    ]} }.

start_child(Name, Filter, Timeout) ->
    {ok, Pid} = supervisor:start_child(?MODULE, [Filter, Timeout]),
    register(Name, Pid),
    {ok, Name}.
