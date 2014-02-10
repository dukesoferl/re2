.PHONY: all clean doc test dialyzer check

REBAR:=$(shell sh -c "PATH='$(PATH)':support which rebar\
	||support/getrebar||echo false")

all:
	$(REBAR) compile eunit

doc:
	$(REBAR) doc

clean:
	$(REBAR) clean

test:
	ERL_LIBS=$(CURDIR) $(REBAR) eunit

dialyzer:
	ERL_LIBS=$(CURDIR) dialyzer -n -nn ebin

check: test dialyzer
