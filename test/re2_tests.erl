%% Copyright 2010-2020 Tuncer Ayaz
%% Use of this source code is governed by a BSD-style
%% license that can be found in the LICENSE file.

-module(re2_tests).

-include_lib("eunit/include/eunit.hrl").

compile_test() ->
    ?assertMatch({ok, _}, re2:compile(".*")),
    ?assertMatch({error, _}, re2:compile("(a")),
    ?assertMatch({error, _}, re2:compile(".*", [{max_mem, 4}])),
    ?assertMatch({'EXIT',{badarg,_}},
                 (catch re2:compile("test(?<name", [unknown]))),
    ?assertMatch({error,{bad_perl_op,_,_}}, re2:compile("test(?<name")).

replace_test() ->
    ?assertEqual(<<"heLo worLd">>,
                 re2:replace("hello world","l+","L",[global])),
    ?assertEqual(<<"heLo world">>, re2:replace("hello world","l+","L")),
    ?assertEqual(error, re2:replace("hello world","k+","L")),
    {ok, RegExR0} = re2:compile("l+"),
    ?assertEqual(<<"heLo worLd">>,
                 re2:replace("hello world",RegExR0,"L",[global])),
    ?assertEqual(<<"heLo world">>, re2:replace("hello world",RegExR0,"L")),
    {ok, RegExR1} = re2:compile("k+"),
    ?assertEqual(error, re2:replace("hello world",RegExR1,"L")),
    ?assertMatch({'EXIT', {badarg,_}},
                 (catch re2:replace("hello world","l+","L",[unknown]))).

run_test() ->
    match_test(run).

match_test() ->
    match_test(match).

match_test(FunName) ->
    {ok, RegExA} = re2:compile(<<"h.*o">>),

    ?assertMatch({'EXIT',{badarg,_}},
                 (catch re2:FunName("hello", RegExA, [ok]))),
    ?assertMatch({'EXIT',{badarg,_}},
                 (catch re2:FunName("hello", "h.*o", [ok]))),
    ?assertMatch({'EXIT',{badarg,_}},
                 (catch re2:FunName("hello", "h(?<name.*o"))),

    ?assertEqual(match, re2:FunName("heLlo", ".*ello",
                                    [caseless,{capture,none}])),
    ?assertMatch({'EXIT',{badarg,_}},
                 (catch re2:FunName("Hello", RegExA, [caseless]))),

    ?assertEqual({match,[<<>>,<<>>,<<>>]},
                 re2:FunName(<<"hello">>, RegExA,
                             [{capture,['A',2,"B"],binary}])),

    ?assertEqual({match,[{-1,0},{-1,0},{-1,0}]},
                 re2:FunName(<<"hello">>, <<"h.*o">>,
                             [{capture,['A',2,"B"],index}])),

    ?assertEqual({match,[<<"ell">>,<<"o">>,<<"h">>]},
                 re2:FunName(<<"hello">>, <<"(?P<B>h)(?P<A>.*)(o)">>,
                             [{capture,['A',3,"B"],binary}])),

    ?assertEqual({match,[{1,3},{4,1},{0,1}]},
                 re2:FunName(<<"hello">>, <<"(?P<B>h)(?P<A>.*)(o)">>,
                             [{capture,['A',3,"B"],index}])),

    ?assertEqual({match,[{0,5}]}, re2:FunName(<<"hello">>,
                                              <<"(?P<A>h)(?P<B>.*)o">>,
                                              [{capture,first,index}])),

    ?assertEqual({match,[<<"hello">>,<<"h">>,<<"ell">>]},
                 re2:FunName(<<"hello">>, <<"(?P<A>h)(?P<B>.*)o">>)),

    ?assertEqual({match,[<<"ello">>,<<>>,<<"ell">>]},
                 re2:FunName(<<"1234ello">>, <<"(?P<A>h?)(?P<B>.*)o">>,
                             [{offset,4}])),

    ?assertEqual({match,[<<>>,<<"ell">>]},
                 re2:FunName(<<"1234ello">>, <<"(?P<A>h?)(?P<B>.*)o">>,
                             [{offset,4},{capture,all_but_first}])),

    ?assertEqual({match,[<<"h">>,<<"ell">>]},
                 re2:FunName(<<"hello">>, <<"(h)(.*)o">>,
                             [{capture,all_but_first,binary}])),

    ?assertEqual({match,[<<"hello">>,<<"h">>,<<"ell">>]},
                 re2:FunName(<<"hello">>, <<"(h)(.*)o">>,
                             [{capture,all,binary}])),

    ?assertEqual({match,[{0,4}]}, re2:FunName(<<"ello">>, <<"(h?)(.*)o">>,
                                              [{capture,first,index}])),

    ?assertEqual({match,[<<"hello">>]},
                 re2:FunName([$h,"e",<<"llo">>], [<<"h.*">>,$o])),

    ?assertEqual(nomatch, re2:FunName(<<"olleh">>,<<"h.*o">>)),

    {ok,RegExB} = re2:compile("abc|(def)"),

    ?assertEqual({match,[<<"abc">>,<<>>]}, re2:FunName("abc", RegExB)),

    ?assertEqual({match,[<<"def">>,<<"def">>]}, re2:FunName("def", RegExB)),

    {ok, RegExC} = re2:compile(<<"h.*o">>, [caseless]),

    ?assertEqual({match, [<<"hElLo">>]},
                 re2:FunName("hElLo", RegExC, [{capture,first,binary}])),

    ?assertEqual({match, [<<"Hello">>]},
                 re2:FunName("Hello", RegExC, [{capture,first,binary}])),

    {ok, RegExD} = re2:compile(<<"h.*o">>),

    ?assertEqual(nomatch, re2:FunName("Hello", RegExD)).
