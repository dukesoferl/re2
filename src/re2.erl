%% Copyright 2010 Tuncer Ayaz. All Rights Reserved.
%% Use of this source code is governed by a BSD-style
%% license that can be found in the LICENSE file.

-module(re2).
-author(tuncerayaz).

-export([new/0,
    match/3,
    match/4
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

new() ->
  ?NIF_NOT_LOADED.
match(_,_,_) ->
  ?NIF_NOT_LOADED.
match(_,_,_,_) ->
  ?NIF_NOT_LOADED.

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

match_test() ->
  {ok, Ref} = new(),

  {match,[<<>>,<<>>,<<>>]} = match(Ref,
    <<"hello">>,
    <<"h.*o">>,
    [{capture,['A',2,"B"],binary}]),

  {match,[{-1,0},{-1,0},{-1,0}]} = match(Ref,
    <<"hello">>,
    <<"h.*o">>,
    [{capture,['A',2,"B"],index}]),

  {match,[<<"ell">>,<<"o">>,<<"h">>]} = match(Ref,
    <<"hello">>,
    <<"(?P<B>h)(?P<A>.*)(o)">>,
    [{capture,['A',3,"B"],binary}]),

  {match,[{0,5}]} = match(Ref,
    <<"hello">>,
    <<"(?P<A>h)(?P<B>.*)o">>,
    [{capture,first,index}]),

  {match,[<<"hello">>,<<"h">>,<<"ell">>]} = match(Ref,
    <<"hello">>,
    <<"(?P<A>h)(?P<B>.*)o">>),

  {match,[<<"ello">>,<<>>,<<"ell">>]} = match(Ref,
    <<"1234ello">>,
    <<"(?P<A>h?)(?P<B>.*)o">>,
    [{offset,4}]),

  {match,[<<>>,<<"ell">>]} = match(Ref,
    <<"1234ello">>,
    <<"(?P<A>h?)(?P<B>.*)o">>,
    [{offset,4},{capture,all_but_first}]),

  {match,[<<"h">>,<<"ell">>]} = match(Ref,
    <<"hello">>,
    <<"(h)(.*)o">>,
    [{capture,all_but_first,binary}]),

  {match,[{0,4}]} = match(Ref,
    <<"ello">>,
    <<"(h?)(.*)o">>,
    [{capture,first,index}]),

  {match,[<<"hello">>]} = match(Ref,
    [$h,"e",<<"llo">>],
    [<<"h.*">>,$o]),

  nomatch = match(Ref,<<"olleh">>,<<"h.*o">>),

  {match,[<<"abc">>,<<>>]} = match(Ref,
    "abc",
    "abc|(def)"),

  {match,[<<"def">>,<<"def">>]} = match(Ref,
    "def",
    "abc|(def)").

-endif.
