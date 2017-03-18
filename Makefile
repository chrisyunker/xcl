REBAR3 = ./bin/rebar3
ELVIS = ./bin/elvis

all: compile

clean: rebar
	@echo "Running rebar clean..."
	$(REBAR3) clean

compile: deps
	@echo "Running rebar compile..."
	$(REBAR3) compile

test: compile
	@echo "Running rebar test..."
	$(REBAR3) eunit skip_deps=true

elvis:
	@echo "Running elvis rock..."
	$(ELVIS) rock

dialyzer:
	@echo "Running rebar dialyzer..."
	$(REBAR3) dialyzer

profile:
	@echo "Running rebar profile..."
	@$(REBAR3) as test compile

edoc:
	@echo "Running rebar edoc..."
	@$(REBAR3) as edoc edoc

xref:
	@echo "Running rebar xref..."
	@$(REBAR3) xref

.PHONY: clean compile test elvis dialyzer profile edoc xref
