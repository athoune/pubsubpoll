-module(psp_client).
-author('mathieu@garambrogne.net').

-behaviour(gen_server2).

%% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2, 
handle_info/2, terminate/2, code_change/3]).

-export([poll/1]).

-record(state, {
    count,
    queue
}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%====================================================================
%% api callbacks
%%====================================================================

start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{
        count = true,
        queue = []
    }}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(poll, _From, State) ->
    psp_count:max(length(State#state.queue)),
    {reply, fetch_data(State#state.queue, State#state.count), State#state{
        queue = []
    }};
handle_call(_Request, _From, State) ->
    {reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({event, Name, EventId}, State) ->
    % case ets:lookup(Name, EventId) of
    %     [] -> {noreply, State};
    %     [{_EventId, _Event}] ->
    %         {noreply, State}
    % end,
    {noreply, State#state{
        queue = [{Name, EventId} | State#state.queue]
    }};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------

poll(ClientPid) ->
    gen_server:call(ClientPid, poll).

%%--------------------------------------------------------------------
%% Private API
%%--------------------------------------------------------------------

fetch_data([], Acc, _Count) ->
    {ok, Acc};
fetch_data([{Name, EventId}|Tail], Acc, Count) ->
    Events = case ets:lookup(Name, EventId) of
        [] ->
            Acc;
        [{_EventId, Event}] ->
            [Event|Acc]
        end,
        case Count of
            true ->
                psp_count:incr();
            _ -> nop
        end,
    
    fetch_data(Tail, Events, Count).
fetch_data(Ids, Count) ->
    fetch_data(Ids, [], Count).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
    client_test() ->
        {ok, ClientPid} = psp_client:start_link(),
        Name = client_test,
        Id = 42,
        Event = toto,
        ets:new(Name, [set, named_table]),
        true = ets:insert(client_test, {Id, Event}),
        gen_server:cast(ClientPid, {event, Name, Id}),
        {ok, Events} = poll(ClientPid),
        ?assertEqual([Event], Events).
-endif.
