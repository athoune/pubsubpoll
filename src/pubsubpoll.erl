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
    supervisor:start_child(pubsubpoll_sup, {psp_channel,
        {psp_channel, start_link, [Filter, TimeOut]},
        permanent, 5000, worker, [psp_channel]}).

channels()->
    gen_server:call(psp_pubsub, channels).

publish(Event) ->
    gen_server:cast(psp_pubsub, {publish, Event}).