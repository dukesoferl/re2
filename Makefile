.PHONY: all clean debug edoc test dialyze check eunit pages rebar3-ebin-copy

REBAR=`sh -c "PATH='$(PATH)':dev which rebar3||dev/getrebar||echo false"`

all:
	@$(REBAR) compile

debug:
	@sh -c "RE2_DEBUG=1 $(REBAR) as debug compile"

pages: edoc
	@test -d pages/out/edoc && rm pages/out/edoc/* || mkdir -p pages/out/edoc
	@cp doc/*.html doc/*.css doc/*.png pages/out/edoc
	@(cd pages && ./export)
	@rm -f pages/out/*.html~

edoc:
	@$(REBAR) edoc

clean:
	@$(REBAR) clean
	@rm -fr pages/out/edoc
	@rm -f pages/out/*.html

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
