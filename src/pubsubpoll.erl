-module(pubsubpoll).
-author('mathieu@garambrogne.net').

-export([
    create_channel/2,
    create_channel/3,
    channels/0,
    publish/1
]).

create_channel(Name, Filter) ->
    create_channel(Name, Filter, 90000).

create_channel(Name, Filter, TimeOut) ->
    psp_channel_sup:start_child(Name, Filter, TimeOut).

channels()->
    gen_server:call(psp_pubsub, channels).

publish(Event) ->
    gen_server:cast(psp_pubsub, {publish, Event}).