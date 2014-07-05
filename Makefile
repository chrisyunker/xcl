.PHONY: clean
PLT_LIBS = erts kernel stdlib sasl crypto public_key compiler runtime_tools ssl
PLT = .xcl.plt
DIALYZER_APPS_PATHS = ebin deps/*/ebin

all: compile

get-deps: rebar
	./rebar get-deps

compile: get-deps
	./rebar compile

app:
	./rebar compile skip_deps=true -f

test: compile
	./rebar eunit skip_deps=true

clean: rebar
	./rebar clean

dialyzer: compile $(PLT)
	@dialyzer -Wno_return --fullpath --plt $(PLT) $(DIALYZER_APPS_PATHS) | grep -v -f dialyzer.ignore-warnings

check_plt: all
	@dialyzer --check_plt --plt $(PLT) --apps $(PLT_LIBS)

clean_plt:
	rm $(PLT)

$(PLT):
	@dialyzer --build_plt --output_plt $(PLT) --apps $(PLT_LIBS)

xref:
	@./priv/xref_check.es | grep -v "unresolved call" || true

rebar:
	wget http://cloud.github.com/downloads/basho/rebar/rebar && chmod u+x rebar

