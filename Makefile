.PHONY: all dev clean edoc deps test plt dialyze check eunit pages

REBAR=`sh -c "PATH='$(PATH)':dev which rebar||dev/getrebar||echo false"`

all:
	@$(REBAR) compile

debug:
	@sh -c "DEBUG=1 $(REBAR) compile -DDEV -DDEBUG"

dev:
	@$(REBAR) compile -DDEV

pages: edoc
	@test -d pages/out/edoc && rm pages/out/edoc/* || mkdir -p pages/out/edoc
	@cp doc/*.html doc/*.css doc/*.png pages/out/edoc
	@(cd pages && ./export)
	@rm -f pages/out/*.html~

edoc:
	@$(REBAR) doc

clean:
	@$(REBAR) clean
	@rm -fr pages/out/edoc
	@rm -f pages/out/*.html

distclean: clean
	@rm -fr c_src/re2

check: test dialyze

deps:
	@sh -c "RE2_TEST_DEPS=1 $(REBAR) prepare-deps"

test: eunit

eunit:
	@sh -c "RE2_TEST_DEPS=1 $(REBAR) eunit"

plt:
	@sh -c "$(REBAR) -vv check-plt || $(REBAR) -vv build-plt"

dialyze:
	@$(REBAR) -vv dialyze
