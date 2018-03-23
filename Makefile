ERL=erl
REBAR=./rebar
BEAMDIR=./deps/*/ebin ./ebin

.PHONY: deps doc test
compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean

deps:
	@$(REBAR) get-deps
	@$(REBAR) update-deps

xref:
	@$(REBAR) xref skip_deps=true

doc:
	@$(REBAR) skip_deps=true doc

test:
	@rm -rf .eunit
	@mkdir -p .eunit
	@ERL_FLAGS="-config -config test.config" $(REBAR) skip_deps=true eunit

run:
	@$(ERL) -pa ebin -pa deps/*/ebin -config test.config -s worker_pool start

