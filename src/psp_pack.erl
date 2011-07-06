-module(psp_pack).
-author('mathieu@garambrogne.net').

-export([pack/2, unpack/1]).

pack(Channel, Event) ->
    <<Channel:16, Event:64>>.

unpack(Pack) ->
    <<Channel:16, Event:64>> = Pack,
    {Channel, Event}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

    pack_test() ->
        ?assertEqual(<<0,16,0,0,0,0,0,0,18,199>>, pack(16, 4807)).

    unpack_test() ->
        ?assertEqual({16, 4807}, unpack(<<0,16,0,0,0,0,0,0,18,199>>)).

-endif.
