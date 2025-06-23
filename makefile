all: build/run_game.svg build/overview.svg

build:
	mkdir $@

build/%.svg: documentation/%.dot build
	dot -Tsvg $< > $@

run:
	devenv processes up -d

sql-lint:
	sqlfluff lint --config .sqlfluff --dialect postgres --format human .

clean:
	process-compose down || true
	rm -rf ${STATEDIR}/minio
	rm -rf ${STATEDIR}/db

################################################################################
# Test
################################################################################

test: test/contracts/examples

#########################################
# JSON Contracts
#########################################

# Creates a makefile target for validating json contract examples
test/contracts/examples/%: documentation/contracts/examples/%
	jv $(subst /examples/,/,$(subst -example.json,.json,$<)) $<

CONTRACTS_EXAMPLES := $(subst documentation,test,$(wildcard documentation/contracts/examples/*.json))

#.PHONY: $(CONTRACTS_EXAMPLES) # idk why including this line doesn't work

# Tests all json contract examples are valid by their contracts
.PHONY: test/contracts/examples
test/contracts/examples: $(CONTRACTS_EXAMPLES)

