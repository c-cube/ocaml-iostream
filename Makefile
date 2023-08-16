
DUNE_OPTS?=
build:
	dune build @install $(DUNE_OPTS)

clean:
	@dune clean

test:
	@dune runtest $(DUNE_OPTS)

doc:
	@dune build $(DUNE_OPTS) @doc

WATCH?= @check @runtest
watch:
	dune build $(DUNE_OPTS) -w $(WATCH)

.PHONY: test clean watch build
