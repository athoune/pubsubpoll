-module(pubsubpoll).
-author('mathieu@garambrogne.net').

-export([
    create_channel/1,
    create_channel/2,
    publish/1
]).

create_channel(Filter) ->
    create_channel(Filter, 90000).

create_channel(Filter, TimeOut) ->
    supervisor:start_child(pubsubpoll_sup, {psp_channel,
        {psp_channel, start_link, [Filter, TimeOut]},
        permanent, 5000, worker, [psp_channel]}).

publish(_Event) ->
    ok.