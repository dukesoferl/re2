%% Copyright 2010 Tuncer Ayaz. All Rights Reserved.
%% Use of this source code is governed by a BSD-style
%% license that can be found in the LICENSE file.

-module(re2).
-author(tuncerayaz).

-export([
         compile/1,
         compile/2,
         match/2,
         match/3
        ]).

-on_load(init/0).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(NIF_NOT_LOADED, "NIF library not loaded").

init() ->
  case code:priv_dir(re2) of
    {error, bad_name} ->
      SoName = filename:join("../priv", re2_nif);
    Dir ->
      SoName = filename:join(Dir, re2_nif)
  end,
  erlang:load_nif(SoName, 0).

compile(_) ->
  ?NIF_NOT_LOADED.
compile(_,_) ->
  ?NIF_NOT_LOADED.
match(_,_) ->
  ?NIF_NOT_LOADED.
match(_,_,_) ->
  ?NIF_NOT_LOADED.

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

match_test() ->
  {ok, RegExA} = compile(<<"h.*o">>),

  {error,undefined_option} = compile("test(?<name", [undefined]),
  {error,undefined_option} = match("hello", RegExA, [ok]),
  {error,undefined_option} = match("hello", "h.*o", [ok]),

  {error,{re2_obj_not_ok,bad_perl_op,"invalid perl operator: (?<","(?<"}} =
    compile("test(?<name"),
  {error,{re2_obj_not_ok,bad_perl_op,"invalid perl operator: (?<","(?<"}} =
    match("foobar", "test(?<name"),

  {match,[<<>>,<<>>,<<>>]} = match(
    <<"hello">>,
    RegExA,
    [{capture,['A',2,"B"],binary}]),

  {match,[<<>>,<<>>,<<>>]} = match(
    <<"hello">>,
    RegExA,
    [{capture,['A',2,"B"],binary}]),

  {match,[{-1,0},{-1,0},{-1,0}]} = match(
    <<"hello">>,
    <<"h.*o">>,
    [{capture,['A',2,"B"],index}]),

  {match,[<<"ell">>,<<"o">>,<<"h">>]} = match(
    <<"hello">>,
    <<"(?P<B>h)(?P<A>.*)(o)">>,
    [{capture,['A',3,"B"],binary}]),

  {match,[{0,5}]} = match(
    <<"hello">>,
    <<"(?P<A>h)(?P<B>.*)o">>,
    [{capture,first,index}]),

  {match,[<<"hello">>,<<"h">>,<<"ell">>]} = match(
    <<"hello">>,
    <<"(?P<A>h)(?P<B>.*)o">>),

  {match,[<<"ello">>,<<>>,<<"ell">>]} = match(
    <<"1234ello">>,
    <<"(?P<A>h?)(?P<B>.*)o">>,
    [{offset,4}]),

  {match,[<<>>,<<"ell">>]} = match(
    <<"1234ello">>,
    <<"(?P<A>h?)(?P<B>.*)o">>,
    [{offset,4},{capture,all_but_first}]),

  {match,[<<"h">>,<<"ell">>]} = match(
    <<"hello">>,
    <<"(h)(.*)o">>,
    [{capture,all_but_first,binary}]),

  {match,[{0,4}]} = match(
    <<"ello">>,
    <<"(h?)(.*)o">>,
    [{capture,first,index}]),

  {match,[<<"hello">>]} = match(
    [$h,"e",<<"llo">>],
    [<<"h.*">>,$o]),

  nomatch = match(<<"olleh">>,<<"h.*o">>),

  {ok,RegExB} = compile("abc|(def)"),

  {match,[<<"abc">>,<<>>]} = match(
    "abc",
    RegExB),

  {match,[<<"def">>,<<"def">>]} = match(
    "def",
    RegExB),

  {ok, RegExC} = compile(<<"h.*o">>, [caseless]),

  {match, [<<"hElLo">>]} = match(
    "hElLo",
    RegExC,
    [{capture,first,binary}]),

  {match, [<<"Hello">>]} = match(
    "Hello",
    RegExC,
    [{capture,first,binary}]),

  {ok, RegExD} = compile(<<"h.*o">>),

  nomatch = match(
    "Hello",
    RegExD).

-endif.
