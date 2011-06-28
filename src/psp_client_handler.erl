-module(psp_client_handler).
-author('mathieu@garambrogne.net').

-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
handle_info/2, terminate/2, code_change/3]).

-record(state, {client}).

init([Client]) ->
    error_logger:info_msg("Starting client handler : ~p~n", [Client]),
    {ok, #state{
        client = Client
    }}.

handle_event({event, Event}, State) ->
    io:format("client ~w : ~w~n", [State#state.client, Event]),
    {ok, State};
handle_event(_Event, State) ->
    {ok, State}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
