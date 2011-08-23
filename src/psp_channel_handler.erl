-module(psp_channel_handler).
-author('mathieu@garambrogne.net').

-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-export([suscribe/2, clients/1]).

-record(state, {
    client
}).

% Public API

suscribe(Client, Channel) ->
    gen_event:add_handler(Channel, {psp_channel_handler, Client}, [Client]).

publish(Channel, Event) ->
    % [TODO] insert event in ets
    % Id = proplists:get_value('_id', Event),
    % true = ets:insert(State#state.name, {Id, Event}),
    gen_event:notify(Channel, {event, Event}).

clients(Channel) ->
    lists:map(fun({psp_channel_handler, Pid}) -> Pid end, gen_event:which_handlers(Channel)).

% Gen_event API

% [TODO] : initialize ets table for each channel
% Name = ets:new(Name, [set, protected, named_table]),
% [TODO] : garbage collector with a spawned timer? a gen_server?

% handler parts

init([Client]) ->
    {ok, #state{client = Client}}.

handle_event({event, _} = Event, #state{client= Client} = State) ->
    gen_server:cast(Client, Event),
    {ok, State};

handle_event(_Event, State) ->
    {ok, State}.

handle_call(_Request, State) ->
    {ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Arg, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Private API

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

psp_channel_test() ->
    {ok, Channel} = gen_event:start_link(),
    {ok, Client} = psp_client:start_link(),
    suscribe(Client, Channel),
    ?assertEqual([Client], clients(Channel)),
    publish(Channel, {"toto", 42}),
    timer:sleep(1),
    {ok, Events} = psp_client:poll(Client),
    error_logger:info_msg("Client/Channel: ~p~n", [Events]).

-endif.
