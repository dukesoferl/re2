.PHONY: all dev clean doc test plt dialyze check eunit qc

REBAR=`sh -c "PATH='$(PATH)':support which rebar\
	||support/getrebar||echo false"`

all:
	@$(REBAR) compile

debug:
	@sh -c "DEBUG=1 $(REBAR) compile -DDEV -DDEBUG"

dev:
	@$(REBAR) compile -DDEV

doc:
	@$(REBAR) doc

clean:
	@$(REBAR) clean

distclean: clean
	@rm -fr c_src/re2

check: test dialyze

deps:
	@sh -c "RE2_TEST_DEPS=1 $(REBAR) prepare-deps"

test: eunit qc

eunit:
	@$(REBAR) eunit

qc:
	@$(REBAR) qc

plt:
	@sh -c "$(REBAR) -vv check-plt || $(REBAR) -vv build-plt"

dialyze:
	@$(REBAR) -vv dialyze
