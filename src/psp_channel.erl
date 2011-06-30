-module(psp_channel).
-author('mathieu@garambrogne.net').

-behaviour(gen_server).

%% gen_server callbacks
-export([start_link/3, init/1, handle_call/3, handle_cast/2, 
handle_info/2, terminate/2, code_change/3]).

-export([filter/1]).

-record(state, {
    name,
    filter,
    timeout,
    clients,
    events,
    garbage
    }).

%%====================================================================
%% api callbacks
%%====================================================================

start_link(Name, Filter, Timeout) ->
    gen_server:start_link(?MODULE, [Name, Filter, Timeout], []).

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
init([Name, Filter, Timeout]) ->
    % [TODO] filter can a list of {key, value} or a fun
    error_logger:info_msg("Starting channel ~w~n", [Name]),
    ets:new(Name, [set, protected, named_table]),
    ok = gen_server:cast(psp_pubsub, {new_channel, self(), Filter}),
    {ok, #state{
        name = Name,
        filter = Filter,
        timeout = Timeout,
        clients = [],
        events = [],
        garbage = 0
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
handle_call(filter, _From, State) ->
    {reply, {ok, State#state.filter}, State};
handle_call(_Request, _From, State) ->
    {reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({event, Event}, State) ->
    %error_logger:info_msg("Chan ~w got event ~p~n", [self(), Event]),
    %[TODO] Filtering event and propagate it to clients.
    Id = proplists:get_value('_id', Event),
    true = ets:insert(State#state.name, {Id, Event}),
    broadcast(State#state.name, Id, State#state.clients),
    {NewEvents, Garbage} = case State#state.garbage of
        50 ->
            {_, M, S} = now(),
            Now = M * 1000 + S,
            {CleanEvents, Trash} = garbage(State#state.events, Now - State#state.timeout),
            error_logger:info_msg("Trash ~w~n", [Trash]),
            ok = clean(State#state.name, Trash),
            {CleanEvents, 0};
        _ ->
            {State#state.events, State#state.garbage + 1}
        end,
    {noreply, State#state{
        events = [Id | NewEvents],
        garbage = Garbage
    }};
handle_cast({suscribe, ClientPid}, State) ->
    {noreply, State#state{
        clients = [ClientPid | State#state.clients]
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

filter(Channel) ->
    gen_server:call(Channel, filter).

%%--------------------------------------------------------------------
%% Private API
%%--------------------------------------------------------------------

broadcast(_Name, _EventId, []) ->
    ok;
broadcast(Name, EventId, [Client | Tail]) ->
    gen_server:cast(Client, {event, Name, EventId}),
    broadcast(Name, EventId, Tail).

garbage([], _Since, Trash) ->
    {[], Trash};
garbage([Event|Events] = All, Since, Trash) ->
    if
        Event < Since ->
            garbage(Events, Since, [Event | Trash]);
        true ->
            {lists:reverse(All), Trash}
    end.
garbage([], _) ->
    {[], []};
garbage(Events, Since) ->
    garbage(lists:reverse(Events), Since, []).

clean(_Tab, []) ->
    ok;
clean(Tab, [Id|Tail])->
    true = ets:delete(Tab, Id),
    clean(Tab, Tail).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

garbage_test() ->
    A = [62,57,51,50,45],
    {AA, Trash} = garbage(A, 51),
    ?assertEqual([50, 45], Trash),
    ?assertEqual([62,57,51], AA).

-endif.