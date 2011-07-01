-module(psp_test).

-include_lib("eunit/include/eunit.hrl").

flood_test() ->
    %{tiemout, 100,
    %fun() ->
        ok = application:start(pubsubpoll),
        psp_count:start_link(300000),
        {ok, _ChanA} = pubsubpoll:create_channel(chan_a, {name, "Robert"}),
        {ok, _ChanB} = pubsubpoll:create_channel(chan_b, {}),
        io:format("Chan: ~p~n", [pubsubpoll:channels()]),
        clients(300),
        flood(1000),
        timer:sleep(10000),
        Count = psp_count:value(),
        ?assertEqual(Count, 0),
        ?debugFmt("Count ~w~n", [Count]),
        ok.
    %end}.

flood(0) ->
    ok;
flood(Cpt) ->
    ok = pubsubpoll:publish([{name, "Robert"}, {flavor, "Strawberry"}]),
    flood(Cpt -1).

clients(Cpt) ->
    clients(Cpt, []).
clients(0, Clients) ->
    {ok, Clients};
clients(Cpt, Clients) ->
    {ok, ClientPid} = pubsubpoll:create_client(),
    pubsubpoll:suscribe(ClientPid, chan_a),
    clients(Cpt-1, [ClientPid | Clients]).