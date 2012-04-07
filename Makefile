.PHONY: all clean doc test

REBAR=@`sh -c "PATH='$(PATH)':support which rebar\
	||support/getrebar||echo false"`

all:
	$(REBAR) compile eunit

doc:
	$(REBAR) doc

clean:
	$(REBAR) clean

test:
	$(REBAR) eunit
