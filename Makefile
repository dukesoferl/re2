.PHONY: all clean test

REBAR=$(shell sh -c "PATH='$(PATH)':support which rebar||support/getrebar||echo false")

all:
	$(REBAR) compile eunit

clean:
	$(REBAR) clean

test:
	$(REBAR) eunit
