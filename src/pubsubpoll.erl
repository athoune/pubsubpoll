-module(pubsubpoll).
-author('mathieu@garambrogne.net').

-export([
    create_channel/1,
    create_channel/2,
    channels/0,
    publish/1
]).

create_channel(Filter) ->
    create_channel(Filter, 90000).

create_channel(Filter, TimeOut) ->
    psp_channel_sup:start_child(Filter, TimeOut).

channels()->
    gen_server:call(psp_pubsub, channels).

publish(Event) ->
    gen_server:cast(psp_pubsub, {publish, Event}).