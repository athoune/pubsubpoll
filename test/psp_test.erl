-module(psp_test).

-include_lib("eunit/include/eunit.hrl").

flood_test() ->
    ok = application:start(pubsubpoll),
    psp_count:start_link(),
    {ok, _ChanA} = pubsubpoll:create_channel(chan_a, {name, "Robert"}),
    {ok, _ChanB} = pubsubpoll:create_channel(chan_b, {}),
    io:format("Chan: ~p~n", [pubsubpoll:channels()]),
    clients(200),
    flood(1000),
    timer:sleep(4000),
    ?debugFmt("Count ~w~n", [psp_count:value()]).

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