%% Copyright 2013-2020 Tuncer Ayaz
%% Use of this source code is governed by a BSD-style
%% license that can be found in the LICENSE file.
%
-module(re2_qc).

%%
%% Triq is broken so there is no way to make tests working so just disable all of them until they
%% are fixed using something different.
%%

%
%% We aim to test the NIFs and not RE2 itself, so the properties are
%% only concerned with making sure the Erlang interface works as
%% documented.
%
%-include_lib("triq/include/triq.hrl").
%-triq(eunit).
%
%prop_number() ->
%    ?FORALL(Int, non_neg_integer(),
%            begin
%                N = list_to_binary(integer_to_list(Int)),
%                {match, [N]} =:= re2:match(N, "^[0-9]+$")
%            end).
%
%prop_byte() ->
%    ?FORALL(Byte, byte(),
%            match =:= re2:match([Byte], "^.*", [{capture, none}])).
%
%binary_str(Str) -> unicode:characters_to_binary(Str).
%
%prop_char() ->
%    ?FORALL(Char, unicode_char(),
%            begin
%                C = unicode:characters_to_binary([Char]),
%                match =:= re2:match(C, "^.+$", [{capture, none}])
%            end).
%
%prop_string() ->
%    ?FORALL(Str, ?LET(Size, choose(1,64), unicode_string(Size)),
%            begin
%                S = binary_str(Str),
%                match =:= re2:match(S, ".*", [{capture, none}])
%            end).
%
%-define(REGEXES,
%        [".*", "([0-7]+)[uUlL]*", "[a|b]", "h.*o", "(\\d+):(\\w+)"]).
%
%-define(REGEXOPTS,
%        [[{capture, none}],
%         [{capture, all_but_first, binary}],
%         [{capture, first, index}],
%         [{capture, first, binary}],
%         [{offset, 1}, {capture, all_but_first, binary}]]).
%
%-define(STRINGS, ["12", "34", "hejsan", "foobar", "baz"]).
%
%random_re_prop({M0, F0}, {M1, F1}) ->
%    ?FORALL({Str,RE,Opts},
%            {elements(?STRINGS), elements(?REGEXES), elements(?REGEXOPTS)},
%            M0:F0(Str, RE, Opts) =:= M1:F1(Str, RE, Opts)).
%
%prop_random_re2() ->
%    random_re_prop({re2, match}, {re2, match}).
%
%prop_random_re_vs_re2() ->
%    random_re_prop({re, run}, {re2, match}).
%
%-define(MIN_MEM, 1100).
%-define(MAX_MEM, 2 bsl 30 - 1).
%compile_options_static() -> elements(['caseless']).
%compile_options_dynamic() -> ?LET(MaxMem, choose(?MIN_MEM, ?MAX_MEM),
%                                  {'max_mem', MaxMem}).
%compile_option() ->
%    oneof([compile_options_static(), compile_options_dynamic()]).
%
%prop_compile() ->
%    ?FORALL({RE, Opts}, {elements(?REGEXES), [compile_option()]},
%            element(1, re2:compile(RE, Opts)) =:= ok).
%
%replace_options() ->
%    elements([[], ['global']]).
%
%prop_replace() ->
%    ?FORALL({Str,RE,Replacement,Opts},
%            {?LET(S, elements(?STRINGS), binary_str(S)),
%             elements(?REGEXES),
%             ?LET(S, unicode_string(64), binary_str(S)),
%             replace_options()},
%            ?IMPLIES(not is_substr(Str, Replacement),
%                     begin
%                         case re2:match(Str, RE, [{capture, none}]) of
%                             match ->
%                                 Res = re2:replace(Str, RE, Replacement, Opts),
%                                 %% Replacement must be a substring of Str
%                                 is_substr(Res, Replacement);
%                             nomatch ->
%                                 true
%                         end
%                     end)).
%
%is_substr(Str1, Str2) ->
%    nomatch =/= binary:match(Str1, Str2).
%
%value_spec_id() -> oneof([non_neg_integer(), atom(), unicode_string()]).
%value_spec_list() -> non_empty(list(value_spec_id())).
%value_spec_static() -> elements(['all', 'all_but_first', 'first', 'none']).
%value_spec() -> oneof([value_spec_static(), value_spec_list()]).
%match_static_opt() -> elements(['caseless']).
%match_offset_opt(Str) -> ?LET(Offset, choose(1, length(Str)),
%                              {'offset', Offset}).
%match_capture_1_opt() -> ?LET(VS, value_spec(), {'capture', VS}).
%match_capture_2_opt() -> ?LET({VS, Opts},
%                              {value_spec(), elements(['index', 'binary'])},
%                              {'capture', VS, Opts}).
%valid_match_option(Str) ->
%    [oneof([match_static_opt(),
%            match_offset_opt(Str),
%            match_capture_1_opt(),
%            match_capture_2_opt()])].
%
%random_re_tuple() ->
%    ?LET({Str, RE}, {elements(?STRINGS), elements(?REGEXES)},
%         ?LET(Opts, valid_match_option(Str),
%              {Str, RE, Opts})).
%
%prop_random_re_spec() ->
%    ?FORALL({Str,RE,Opts}, random_re_tuple(),
%            re2:match(Str, RE, Opts) =:= re2:match(Str, RE, Opts)).
