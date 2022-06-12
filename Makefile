.PHONY: compile test eunit ct shell clean doc elvis

ERLANG = podman run -it --rm -v .:/src -w /src erlang
REBAR = @$(ERLANG) rebar3

compile:
	@$(REBAR) compile

test: dialyzer eunit ct proper
	@$(REBAR) cover

eunit:
	@$(REBAR) eunit

ct:
	@$(REBAR) ct

proper:
	@$(REBAR) proper

dialyzer:
	@$(REBAR) dialyzer

shell:
	@$(REBAR) shell

clean:
	@$(REBAR) clean

distclean: clean
	rm -Rf _build

doc:
	@$(REBAR) edoc

elvis:
	@$(ERLANG) ./elvis rock -V --config elvis.config
