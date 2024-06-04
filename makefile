all: build/run_game.svg build/overview.svg

build:
	mkdir $@

build/%.svg: documentation/%.dot build
	dot -Tsvg $< > $@
