-module(psp_user_test).

-author('mathieu@garambrogne.net').

-behaviour(gen_server2).

-export([poll/1,
    auto_poll/2]).

%% gen_server callbacks
-export([start_link/2, init/1, handle_call/3, handle_cast/2, 
handle_info/2, terminate/2, code_change/3]).

-define(INTERVAL, 50).

-record(state, {
    client,
    onPoll
}).

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------

poll(UserPid) ->
    gen_server:cast(UserPid, poll).

auto_poll(UserPid, Interval) ->
    poll(UserPid),
    timer:apply_after(random:uniform(Interval), ?MODULE, auto_poll, [UserPid, Interval]).



%%====================================================================
%% api callbacks
%%====================================================================

start_link(Client, OnPoll) ->
    gen_server:start_link(?MODULE, [Client, OnPoll], []).

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
init([Client, OnPoll]) ->
    error_logger:info_msg("init user ~w connected to ~w~n", [self(), Client]),
    {ok, #state{
        client = Client,
        onPoll = OnPoll
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
handle_call(_Request, _From, State) ->
    {reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(poll, #state{client = Client, onPoll = OnPoll} = State) ->
    {ok, Events} = psp_client:poll(Client),
    %error_logger:info_msg("user Events ~w~n", [Events]),
    OnPoll(Events),
    %error_logger:info_msg("Events ~w~n", [Events]),
    %metrics_countdown:decr(plop, length(Events)),
    %error_logger:info_msg("poll ~w from ~w~n", [Client, self()]),
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
%% Private API
%%--------------------------------------------------------------------


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
    client_test() ->
        application:start(pubsubpoll),
        {ok, _}         = psp_user_test_sup:start(),
        {ok, chan_a}    = pubsubpoll:create_channel(chan_a, {name, "Robert"}),
        {ok, ClientPid} = pubsubpoll:create_client(),
        ok = pubsubpoll:suscribe(ClientPid, chan_a),
        {ok, UserPid}   = psp_user_test_sup:start_child(ClientPid, fun(Events) ->
            error_logger:info_msg("Wouaaaaaais : ~p~n", [Events]),
            ?assertEqual(1, length(Events))
        end),
        ok = pubsubpoll:publish([{name, "Robert"}, {flavor, "Strawberry"}]),
        timer:sleep(1),
        ok = poll(UserPid),
        ok = pubsubpoll:publish([{name, "Robert"}, {flavor, "Strawberry"}]),
        timer:sleep(1),
        ok = poll(UserPid).
-endif.
