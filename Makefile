
DUNE_OPTS?=
build:
	dune build @install $(DUNE_OPTS)

clean:
	@dune clean

test:
	@dune runtest $(DUNE_OPTS)

doc:
	@dune build $(DUNE_OPTS) @doc

format:
	@dune build $(DUNE_OPTS) @fmt --auto-promote
	@dune format-dune-file dune-project > dune-project.fmt && mv dune-project.fmt dune-project

check-format:
	@dune build $(DUNE_OPTS) @fmt
	@dune format-dune-file dune-project > dune-project.fmt && diff dune-project dune-project.fmt && rm dune-project.fmt

WATCH?= @check @runtest
watch:
	dune build $(DUNE_OPTS) -w $(WATCH)

.PHONY: test clean watch build

VERSION=$(shell awk '/^version:/ {print $$2}' iostream.opam)
update_next_tag:
	@echo "update version to $(VERSION)..."
	sed -i "s/NEXT_VERSION/$(VERSION)/g" $(wildcard src/**/*.ml) $(wildcard src/**/*.mli)
	sed -i "s/NEXT_RELEASE/$(VERSION)/g" $(wildcard src/*.ml) $(wildcard src/**/*.ml) $(wildcard src/*.mli) $(wildcard src/**/*.mli)
