all: build/run_game.svg build/overview.svg

build:
	mkdir $@

build/%.svg: documentation/%.dot build
	dot -Tsvg $< > $@

run:
	devenv processes up -d

clean:
	devenv processes down || true
	rm -r .devenv/state/minio
	rm -r .devenv/state/postgres
