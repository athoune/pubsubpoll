-module(psp_count).
-author('mathieu@garambrogne.net').

-behaviour(gen_server).

%% gen_server callbacks
-export([start_link/1, start_link/2, init/1, handle_call/3, handle_cast/2, 
handle_info/2, terminate/2, code_change/3]).

-export([value/0, incr/0, max/1, max_value/0]).

-record(state, {max, count, timer, action, maxima}).

%%====================================================================
%% api callbacks
%%====================================================================

start_link(Size) ->
    start_link(Size, nil).

start_link(Size, OnFinished) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Size, OnFinished], []).

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
init([Size, OnFinished]) ->
    {ok, #state{
        max    = Size,
        count  = Size,
        timer  = now(),
        action = OnFinished,
        maxima = 0
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
handle_call(value, _From, State) ->
    {reply, State#state.count, State};
handle_call(max_value, _From, State) ->
    {reply, State#state.maxima, State};
handle_call(_Request, _From, State) ->
    {reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(incr, State) ->
    case State#state.count of
        1 ->
            Score = timer:now_diff(now(), State#state.timer),
            error_logger:info_msg("~w in ~w µs~n ~w µs~nMaxima: ~w~n", [
                State#state.max, Score, Score / State#state.max,
                State#state.maxima
            ]),
            case State#state.action of
                nil -> nop;
                Action -> apply(Action, [State])
            end;
        _ ->
            nop
    end,
    case (State#state.count rem 10000) of
        0 ->
            error_logger:info_msg("counting : ~w 10k~n", [State#state.count / 10000]);
        _ ->
            nop
    end,
    {noreply, State#state{
        count = State#state.count - 1
    }};
handle_cast({max, Value}, State) ->
    M = case Value > State#state.maxima of
        true -> Value;
        _ -> State#state.maxima
    end,
    {noreply, State#state{ maxima = M}};
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

value() ->
    gen_server:call(?MODULE, value).

incr() ->
    gen_server:cast(?MODULE, incr).

max(Value) ->
    gen_server:cast(?MODULE, {max, Value}).

max_value() ->
    gen_server:call(?MODULE, max_value).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

count_test() ->
    psp_count:start_link(1, fun(_State) -> 
        ?assertEqual(ok, ok)
    end),
    psp_count:incr().

-endif.
