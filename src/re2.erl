%% Copyright 2010-2011 Tuncer Ayaz. All Rights Reserved.
%% Use of this source code is governed by a BSD-style
%% license that can be found in the LICENSE file.

-module(re2).
-author(tuncerayaz).

-export([
         compile/1,
         compile/2,
         match/2,
         match/3,
         replace/3,
         replace/4
        ]).

-on_load(load_nif/0).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(nif_stub,nif_stub_error(?LINE)).
nif_stub_error(Line) ->
  erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

load_nif() ->
  PrivDir = case code:priv_dir(?MODULE) of
              {error, _} ->
                EbinDir = filename:dirname(code:which(?MODULE)),
                AppPath = filename:dirname(EbinDir),
                filename:join(AppPath, "priv");
              Path ->
                Path
            end,
  erlang:load_nif(filename:join(PrivDir, "re2_nif"), 0).

compile(_) ->
  ?nif_stub.
compile(_,_) ->
  ?nif_stub.
match(_,_) ->
  ?nif_stub.
match(_,_,_) ->
  ?nif_stub.
replace(_,_,_) ->
  ?nif_stub.
replace(_,_,_,_) ->
  ?nif_stub.

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

replace_test() ->
  <<"heLo worLd">> = replace("hello world","l+","L",[global]),
  <<"heLo world">> = replace("hello world","l+","L"),
  {error,replace} = replace("hello world","k+","L"),
  {ok, RegExR0} = compile("l+"),
  <<"heLo worLd">> = replace("hello world",RegExR0,"L",[global]),
  <<"heLo world">> = replace("hello world",RegExR0,"L"),
  {ok, RegExR1} = compile("k+"),
  {error,replace} = replace("hello world",RegExR1,"L"),
  {'EXIT',{badarg,_}} = (catch replace("hello world","l+","L",[unknown])).

match_test() ->
  {ok, RegExA} = compile(<<"h.*o">>),

  {'EXIT',{badarg,_}} = (catch compile("test(?<name", [unknown])),
  {error,{bad_perl_op,_,_}} = compile("test(?<name"),
  {'EXIT',{badarg,_}} = (catch match("hello", RegExA, [ok])),
  {'EXIT',{badarg,_}} = (catch match("hello", "h.*o", [ok])),
  {'EXIT',{badarg,_}} = (catch match("hello", "h(?<name.*o")),

  match = match("heLlo", ".*ello", [caseless,{capture,none}]),
  {'EXIT',{badarg,_}} = (catch match("Hello", RegExA, [caseless])),

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

  {match,[<<"hello">>,<<"h">>,<<"ell">>]} = match(
    <<"hello">>,
    <<"(h)(.*)o">>,
    [{capture,all,binary}]),

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
