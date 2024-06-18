all: build/run_game.svg build/overview.svg

build:
	mkdir $@

build/%.svg: documentation/%.dot build
	dot -Tsvg $< > $@

run:
	devenv processes up -d
	sleep 10
	mc mb local/bots
	mc mb local/games
	bash ./add_player.sh
	bash ./game-pipeline.sh

clean:
	devenv processes down || true
	rm -r .devenv/state/minio
	rm -r .devenv/state/postgres
