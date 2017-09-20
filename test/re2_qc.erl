%% Copyright (c) 2013-2017 Tuncer Ayaz. All Rights Reserved.
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

binary_str(Str) -> unicode:characters_to_binary(Str).

prop_char() ->
    ?FORALL(Char, unicode_char(),
            begin
                C = unicode:characters_to_binary([Char]),
                match =:= re2:match(C, "^.+$", [{capture, none}])
            end).

prop_string() ->
    ?FORALL(Str, ?LET(Size, choose(1,64), unicode_string(Size)),
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
            {elements(?STRINGS), elements(?REGEXES), elements(?REGEXOPTS)},
            M0:F0(Str, RE, Opts) =:= M1:F1(Str, RE, Opts)).

prop_random_re2() ->
    random_re_prop({re2, match}, {re2, match}).

prop_random_re_vs_re2() ->
    random_re_prop({re, run}, {re2, match}).

-define(MIN_MEM, 1100).
-define(MAX_MEM, 2 bsl 30 - 1).
compile_options_static() -> elements(['caseless']).
compile_options_dynamic() -> ?LET(MaxMem, choose(?MIN_MEM, ?MAX_MEM),
                                  {'max_mem', MaxMem}).
compile_option() ->
    oneof([compile_options_static(), compile_options_dynamic()]).

prop_compile() ->
    ?FORALL({RE, Opts}, {elements(?REGEXES), [compile_option()]},
            {ok, <<>>} =:= re2:compile(RE, Opts)).

replace_options() ->
    elements([[], ['global']]).

prop_replace() ->
    ?FORALL({Str,RE,Replacement,Opts},
            {?LET(S, elements(?STRINGS), binary_str(S)),
             elements(?REGEXES),
             ?LET(S, unicode_string(), binary_str(S)),
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

value_spec() -> elements(['all',
                          'all_but_first',
                          'first',
                          'none',
                          value_spec_list()]).
value_spec_list() -> non_empty(list(value_spec_id())).
value_spec_id() -> elements([non_neg_integer(), atom(), unicode_string()]).
valid_match_option(Str) ->
    elements(['caseless',
              {'offset', choose(1,length(Str))},
              {'capture', value_spec()},
              {'capture', value_spec(), elements(['index','binary'])}
             ]).

random_re_tuple() ->
    ?LET({Str, RE}, {elements(?STRINGS), elements(?REGEXES)},
         ?LET(Opts, list(valid_match_option(Str)),
              {Str, RE, Opts})).

%% TODO: This takes quite a while. Check for inefficiencies.
prop_random_re_spec() ->
    ?FORALL({Str,RE,Opts}, random_re_tuple(),
            re2:match(Str, RE, Opts) =:= re2:match(Str, RE, Opts)).

-endif.
