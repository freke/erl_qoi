.PHONY: compile test eunit ct shell clean

REBAR = podman run -it --rm -v .:/src -w /src erlang rebar3

compile:
	@$(REBAR) compile

test: dialyzer eunit ct proper

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
