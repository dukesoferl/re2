%% Copyright 2010-2014 Tuncer Ayaz. All Rights Reserved.
%% Use of this source code is governed by a BSD-style
%% license that can be found in the LICENSE file.
%%
%% @doc Erlang NIF bindings for the
%% <a href="https://code.google.com/p/re2/">re2</a> regex library

-module(re2).
-author(tuncerayaz).

-export([compile/1,
         compile/2,
         match/2,
         match/3,
         replace/3,
         replace/4]).

-export_type([compile_option/0,
              match_option/0,
              replace_option/0]).

-on_load(load_nif/0).

-define(nif_stub, nif_stub_error(?LINE)).
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

load_nif() ->
    PrivDir = case code:priv_dir(?MODULE) of
                  {error, bad_name} ->
                      EbinDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(EbinDir),
                      filename:join(AppPath, "priv");
                  Path ->
                      Path
              end,
    erlang:load_nif(filename:join(PrivDir, "re2_nif"), 0).


-type uncompiled_pattern() :: iodata().
-opaque compiled_pattern() :: binary().
-type input()           :: iodata().
-type pattern()         :: uncompiled_pattern() | compiled_pattern().
-type replacement()     :: iodata().

-type match_option()    :: 'caseless' | {'offset', non_neg_integer()}
                         | {'capture', value_spec()}
                         | {'capture', value_spec(), value_spec_type()}.
-type value_spec()      :: 'all' | 'all_but_first' | 'first' | 'none'
                         | [value_id()].
-type value_spec_type() :: 'index' | 'binary'.
-type value_id()        :: non_neg_integer() | string() | atom().
-type match_result()    :: 'match' | 'nomatch' | {'match', list()}
                         | {'error', atom()}.

-type re2error()        :: {atom(), string(), string()}.
-type compile_option()  :: 'caseless' | {'max_mem', non_neg_integer()}.
-type compile_result()  :: {'ok', compiled_pattern()} | {'error', re2error()}.

-type replace_option()  :: 'global'.
-type replace_result()  :: binary() | {'error', atom()} | 'error'.

-spec compile(Pattern::uncompiled_pattern()) -> compile_result().
compile(_) ->
    ?nif_stub.

-spec compile(Pattern::uncompiled_pattern(),
              Options::[compile_option()]) -> compile_result().
compile(_,_) ->
    ?nif_stub.

-spec match(Input::input(), Pattern::pattern()) -> match_result().
match(_,_) ->
    ?nif_stub.

-spec match(Input::input(), Pattern::pattern(),
            Options::[match_option()]) -> match_result().
match(_,_,_) ->
    ?nif_stub.

-spec replace(Input::input(), Pattern::pattern(),
              Replacement::replacement()) -> replace_result().
replace(_,_,_) ->
    ?nif_stub.

-spec replace(Input::input(), Pattern::pattern(),
              Replacement::replacement(),
              Options::[replace_option()]) -> replace_result().
replace(_,_,_,_) ->
    ?nif_stub.
