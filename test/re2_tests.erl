%% Copyright 2010-2011 Tuncer Ayaz. All Rights Reserved.
%% Use of this source code is governed by a BSD-style
%% license that can be found in the LICENSE file.

-module(re2_tests).

-include_lib("eunit/include/eunit.hrl").

replace_test() ->
  <<"heLo worLd">> = re2:replace("hello world","l+","L",[global]),
  <<"heLo world">> = re2:replace("hello world","l+","L"),
  {error,replace} = re2:replace("hello world","k+","L"),
  {ok, RegExR0} = re2:compile("l+"),
  <<"heLo worLd">> = re2:replace("hello world",RegExR0,"L",[global]),
  <<"heLo world">> = re2:replace("hello world",RegExR0,"L"),
  {ok, RegExR1} = re2:compile("k+"),
  {error,replace} = re2:replace("hello world",RegExR1,"L"),
  {'EXIT',{badarg,_}} = (catch re2:replace("hello world","l+","L",[unknown])).

match_test() ->
  {ok, RegExA} = re2:compile(<<"h.*o">>),

  {'EXIT',{badarg,_}} = (catch re2:compile("test(?<name", [unknown])),
  {error,{bad_perl_op,_,_}} = re2:compile("test(?<name"),
  {'EXIT',{badarg,_}} = (catch re2:match("hello", RegExA, [ok])),
  {'EXIT',{badarg,_}} = (catch re2:match("hello", "h.*o", [ok])),
  {'EXIT',{badarg,_}} = (catch re2:match("hello", "h(?<name.*o")),

  match = re2:match("heLlo", ".*ello", [caseless,{capture,none}]),
  {'EXIT',{badarg,_}} = (catch re2:match("Hello", RegExA, [caseless])),

  {match,[<<>>,<<>>,<<>>]} = re2:match(
    <<"hello">>,
    RegExA,
    [{capture,['A',2,"B"],binary}]),

  {match,[<<>>,<<>>,<<>>]} = re2:match(
    <<"hello">>,
    RegExA,
    [{capture,['A',2,"B"],binary}]),

  {match,[{-1,0},{-1,0},{-1,0}]} = re2:match(
    <<"hello">>,
    <<"h.*o">>,
    [{capture,['A',2,"B"],index}]),

  {match,[<<"ell">>,<<"o">>,<<"h">>]} = re2:match(
    <<"hello">>,
    <<"(?P<B>h)(?P<A>.*)(o)">>,
    [{capture,['A',3,"B"],binary}]),

  {match,[{0,5}]} = re2:match(
    <<"hello">>,
    <<"(?P<A>h)(?P<B>.*)o">>,
    [{capture,first,index}]),

  {match,[<<"hello">>,<<"h">>,<<"ell">>]} = re2:match(
    <<"hello">>,
    <<"(?P<A>h)(?P<B>.*)o">>),

  {match,[<<"ello">>,<<>>,<<"ell">>]} = re2:match(
    <<"1234ello">>,
    <<"(?P<A>h?)(?P<B>.*)o">>,
    [{offset,4}]),

  {match,[<<>>,<<"ell">>]} = re2:match(
    <<"1234ello">>,
    <<"(?P<A>h?)(?P<B>.*)o">>,
    [{offset,4},{capture,all_but_first}]),

  {match,[<<"h">>,<<"ell">>]} = re2:match(
    <<"hello">>,
    <<"(h)(.*)o">>,
    [{capture,all_but_first,binary}]),

  {match,[<<"hello">>,<<"h">>,<<"ell">>]} = re2:match(
    <<"hello">>,
    <<"(h)(.*)o">>,
    [{capture,all,binary}]),

  {match,[{0,4}]} = re2:match(
    <<"ello">>,
    <<"(h?)(.*)o">>,
    [{capture,first,index}]),

  {match,[<<"hello">>]} = re2:match(
    [$h,"e",<<"llo">>],
    [<<"h.*">>,$o]),

  nomatch = re2:match(<<"olleh">>,<<"h.*o">>),

  {ok,RegExB} = re2:compile("abc|(def)"),

  {match,[<<"abc">>,<<>>]} = re2:match(
    "abc",
    RegExB),

  {match,[<<"def">>,<<"def">>]} = re2:match(
    "def",
    RegExB),

  {ok, RegExC} = re2:compile(<<"h.*o">>, [caseless]),

  {match, [<<"hElLo">>]} = re2:match(
    "hElLo",
    RegExC,
    [{capture,first,binary}]),

  {match, [<<"Hello">>]} = re2:match(
    "Hello",
    RegExC,
    [{capture,first,binary}]),

  {ok, RegExD} = re2:compile(<<"h.*o">>),

  nomatch = re2:match(
    "Hello",
    RegExD).
