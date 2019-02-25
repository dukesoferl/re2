.PHONY: all clean debug edoc test dialyze check eunit rebar3-ebin-copy

REBAR=`sh -c "PATH='$(PATH)':dev which rebar3||dev/getrebar||echo false"`

all:
	@$(REBAR) compile

debug:
	@sh -c "RE2_DEBUG=1 $(REBAR) as debug compile"

edoc:
	@$(REBAR) edoc

clean:
	@$(REBAR) clean

distclean: clean
	@rm -fr c_src/re2

check: test dialyze

test: eunit

eunit:
	@$(REBAR) eunit

dialyze:
	@$(REBAR) dialyzer

rebar3-ebin-copy:
	@cp -r _build/default/lib/re2/ebin .
