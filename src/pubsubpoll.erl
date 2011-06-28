-module(pubsubpoll).
-author('mathieu@garambrogne.net').

-export([
    create_channel/2,
    create_channel/3,
    channels/0,
    publish/1,
    create_client/0,
    suscribe/2,
    unsuscribe/2
]).

create_channel(Name, Filter) ->
    create_channel(Name, Filter, 90000).

create_channel(Name, Filter, TimeOut) ->
    psp_channel_sup:start_child(Name, Filter, TimeOut).

channels()->
    gen_server:call(psp_pubsub, channels).

publish(Event) ->
    gen_server:cast(psp_pubsub, {publish, Event}).

create_client() ->
    psp_client_sup:start_child().

suscribe(Client, Channel) ->
    gen_event:add_handler(Channel, psp_client_handler, [Client]),
    ok.

unsuscribe(Client, Channel) ->
    gen_event:delete_handler(Channel, psp_client_handler, [Client]),
    ok.