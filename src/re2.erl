%% Copyright 2010-2016 Tuncer Ayaz. All Rights Reserved.
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

-export_type([
              compile_option/0,
              match_option/0,
              replace_option/0
             ]).

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

%% NOTE: compiled_regex/0 is not declared as -opaque because:
%% 1. If you declare :: any() as an opaque type, then the compiler will
%%    complain that it's underspecified like this:
%%    Warning: opaque type compiled_regex() is underspecified and
%%    therefore meaningless
%% 2. Opaque types only make sense when exported, and the compiler will
%%    rightly complain about that as follows:
%%    Warning: opaque type compiled_regex() is not exported

-type uncompiled_regex() :: iodata().
-type compiled_regex() :: any().
%% compiled_regex/0 is an opaque datatype containing a compiled regex
%% created by enif_make_resource(). Resources are totally opaque,
%% which means the actual type is undefined. In the current Erlang VM
%% they behave like empty (&lt;&lt;&gt;&gt;) binaries on the Erlang
%% side, but that could change in future releases. Therefore, make no
%% assumptions, and always treat it as an opaque datatype.
-type subject() :: iodata().
-type regex() :: uncompiled_regex() | compiled_regex().
-type replacement() :: iodata().

-type match_option() :: 'caseless' | {'offset', non_neg_integer()}
                      | {'capture', value_spec()}
                      | {'capture', value_spec(), value_spec_type()}.
-type value_spec() :: 'all' | 'all_but_first' | 'first' | 'none'
                    | [value_id()].
-type value_spec_type() :: 'index' | 'binary'.
-type value_id() :: non_neg_integer() | string() | atom().
-type match_result() :: 'match' | 'nomatch' | {'match', list()}
                      | {'error', atom()}.

-type compile_error() :: {'error', {atom(), string(), string()}}.
-type compile_option() :: 'caseless' | {'max_mem', non_neg_integer()}.
-type compile_result() :: {'ok', compiled_regex()} | compile_error().

-type replace_option() :: 'global'.
-type replace_result() :: binary() | {'error', atom()} | 'error'.

-spec compile(uncompiled_regex()) -> compile_result().
compile(_) ->
    ?nif_stub.

-spec compile(uncompiled_regex(), [compile_option()]) -> compile_result().
compile(_,_) ->
    ?nif_stub.

-spec match(subject(), regex()) -> match_result().
match(_,_) ->
    ?nif_stub.

-spec match(subject(), regex(), [match_option()]) -> match_result().
match(_,_,_) ->
    ?nif_stub.

-spec replace(subject(), regex(), replacement()) -> replace_result().
replace(_,_,_) ->
    ?nif_stub.

-spec replace(subject(), regex(), replacement(),
              [replace_option()]) -> replace_result().
replace(_,_,_,_) ->
    ?nif_stub.
