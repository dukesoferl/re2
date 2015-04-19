.PHONY: all clean doc test dialyzer check eunit qc

REBAR=@`sh -c "PATH='$(PATH)':support which rebar\
	||support/getrebar||echo false"`

all:
	@$(REBAR) compile

doc:
	@$(REBAR) doc

clean:
	@$(REBAR) clean

distclean: clean
	@rm -fr c_src/re2

check: test dialyzer

test: eunit qc

eunit:
	@$(REBAR) eunit

qc:
	@$(REBAR) qc

dialyzer:
	@dialyzer -n -nn ebin
