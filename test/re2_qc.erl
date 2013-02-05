%% Copyright 2013-2014 Tuncer Ayaz. All Rights Reserved.
%% Use of this source code is governed by a BSD-style
%% license that can be found in the LICENSE file.

-module(re2_qc).

%% We aim to test the NIFs and not re2 itself, so the properties are only
%% concerned with making sure the Erlang interface works as documented. That
%% means, good properties to test re2 itself (not the NIFs) would probably
%% generate strings from a regex to match and use similar strategies for
%% replace/2.

-ifdef(TRIQ).
-include_lib("triq/include/triq.hrl").

prop_number() ->
    ?FORALL(Int, non_neg_integer(),
            begin
                N = list_to_binary(integer_to_list(Int)),
                {match, [N]} =:= re2:match(N, "^[0-9]+$")
            end).

prop_byte() ->
    ?FORALL(Byte, byte(),
            match =:= re2:match([Byte], "^.*", [{capture, none}])).

valid_string() -> non_empty(list(oneof([unicode_char(), unicode_binary()]))).

binary_str(Str) -> unicode:characters_to_binary(Str).

prop_char() ->
    ?FORALL(Char, unicode_char(),
            begin
                C = unicode:characters_to_binary([Char]),
                match =:= re2:match(C, "^.+$", [{capture, none}])
            end).

prop_string() ->
    ?FORALL(Str, valid_string(),
            begin
                S = binary_str(Str),
                match =:= re2:match(S, "^.+$", [{capture, none}])
            end).

-define(REGEXES,
        [".*", "([0-7]+)[uUlL]*", "[a|b]", "h.*o", "(\\d+):(\\w+)"]).

-define(REGEXOPTS,
        [[{capture, none}],
         [{capture, all_but_first, binary}],
         [{capture, first, index}],
         [{capture, first, binary}],
         [{offset, 1}, {capture, all_but_first, binary}]]).

-define(STRINGS, ["12", "34", "hejsan", "foobar", "baz"]).

random_re_prop({M0, F0}, {M1, F1}) ->
    ?FORALL({Str,RE,Opts},
            {oneof(?STRINGS), oneof(?REGEXES), oneof(?REGEXOPTS)},
            M0:F0(Str, RE, Opts) =:= M1:F1(Str, RE, Opts)).

prop_random_re2() ->
    random_re_prop({re2, match}, {re2, match}).

prop_random_re_vs_re2() ->
    random_re_prop({re, run}, {re2, match}).

-define(MIN_MEM, 1100).
-define(MAX_MEM, 2 bsl 30 - 1).
sanitize_max_mem(Opts) ->
    lists:map(fun({max_mem, M}) when M < ?MIN_MEM -> {max_mem, ?MIN_MEM};
                 ({max_mem, M}) when M > ?MAX_MEM -> {max_mem, ?MAX_MEM};
                 (E) -> E
              end, Opts).

compile_option() ->
    oneof(['caseless', {'max_mem', non_neg_integer()}]).

prop_compile() ->
    ?FORALL({RE, Opts}, {oneof(?REGEXES),
                         ?LET(Opts, list(compile_option()),
                              sanitize_max_mem(Opts))},
            {ok, <<>>} =:= re2:compile(RE, Opts)).

replace_options() ->
    oneof([[], ['global']]).

prop_replace() ->
    ?FORALL({Str,RE,Replacement,Opts},
            {?LET(S, oneof(?STRINGS), binary_str(S)),
             oneof(?REGEXES),
             ?LET(S, valid_string(), binary_str(S)),
             replace_options()},
            ?IMPLIES(not is_substr(Str, Replacement),
                     begin
                         case re2:match(Str, RE, [{capture, none}]) of
                             match ->
                                 Res = re2:replace(Str, RE, Replacement, Opts),
                                 %% Replacement must be a substring of Str
                                 is_substr(Res, Replacement);
                             nomatch ->
                                 true
                         end
                     end)).

is_substr(Str1, Str2) ->
    nomatch =/= binary:match(Str1, Str2).

value_spec() -> oneof(['all',
                       'all_but_first',
                       'first',
                       'none',
                       value_spec_list()]).
value_spec_list() -> non_empty(list(value_spec_id())).
value_spec_id() -> oneof([non_neg_integer(), atom(), valid_string()]).
valid_match_option(Str) ->
    oneof(['caseless',
           {'offset', choose(1,length(Str))},
           {'capture', value_spec()},
           {'capture', value_spec(), oneof(['index','binary'])}
          ]).

random_re_tuple() ->
    ?LET({Str, RE}, {oneof(?STRINGS), oneof(?REGEXES)},
         ?LET(Opts, list(valid_match_option(Str)),
              {Str, RE, Opts})).

%% TODO: This takes quite a while. Check for inefficiencies.
prop_random_re_spec() ->
    ?FORALL({Str,RE,Opts}, random_re_tuple(),
            re2:match(Str, RE, Opts) =:= re2:match(Str, RE, Opts)).

-endif.
