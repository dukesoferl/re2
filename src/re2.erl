%% Copyright 2010 Tuncer Ayaz. All Rights Reserved.
%% Use of this source code is governed by a BSD-style
%% license that can be found in the LICENSE file.

-module(re2).
-author('tuncerayaz@gmail.com').

-export([new/0,
    match/3]).

-on_load(init/0).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

init() ->
  case code:priv_dir(re2) of
    {error, bad_name} ->
      SoName = filename:join("../priv", re2_nif);
    Dir ->
      SoName = filename:join(Dir, re2_nif)
  end,
  erlang:load_nif(SoName, 0).

new() ->
  "NIF library not loaded".

match(_,_,_) ->
  "NIF library not loaded".

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

basic_test() ->
  {ok, Ref} = new(),
  {match,M0a} = match(Ref,<<"hello">>,<<"(?P<A>h)(?P<B>.*)o">>),
  io:format(user,"M0a ~p~n", [M0a]),
  {match,M0b} = match(Ref,<<"ello">>,<<"(?P<A>h?)(?P<B>.*)o">>),
  io:format(user,"M0b ~p~n", [M0b]),
  {match,M1a} = match(Ref,<<"hello">>,<<"(h)(.*)o">>),
  io:format(user,"M1a ~p~n", [M1a]),
  {match,M1b} = match(Ref,<<"ello">>,<<"(h?)(.*)o">>),
  io:format(user,"M1b ~p~n", [M1b]),
  {match,M2a} = match(Ref,<<"hello">>,<<"h.*o">>),
  io:format(user,"M2a ~p~n", [M2a]),
  {match,M2b} = match(Ref,[$h,"e",<<"llo">>],[<<"h.*">>,$o]),
  io:format(user,"M2b ~p~n", [M2b]),
  nomatch = match(Ref,<<"olleh">>,<<"h.*o">>),
  {match,M3a} = match(Ref,"abc","abc|(def)"),
  io:format(user,"M3a ~p~n", [M3a]),
  {match,M3b} = match(Ref,"def","abc|(def)"),
  io:format(user,"M3b ~p~n", [M3b]).

-endif.
