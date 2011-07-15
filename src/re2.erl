%% Copyright 2010-2011 Tuncer Ayaz. All Rights Reserved.
%% Use of this source code is governed by a BSD-style
%% license that can be found in the LICENSE file.

-module(re2).
-author(tuncerayaz).

-export([compile/1,
         compile/2,
         match/2,
         match/3,
         replace/3,
         replace/4]).

-on_load(load_nif/0).

-define(nif_stub, nif_stub_error(?LINE)).
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
