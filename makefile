all: build/diagram.svg

build:
	mkdir $@

build/%.svg: documentation/%.dot build
	dot -Tsvg $< > $@
