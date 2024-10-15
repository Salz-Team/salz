# Contracts

Most communication between services/programs is done in JSON. In
particular, communication between the following is done in JSON:

- the bots and the game engine
- the match handler and the visualizer
- the game engine and the match handler

For each of these interactions, we have JSON schemas in the
`documentation/contracts` folder. You can view examples of valid JSON
messages in the `documentation/contracts/examples` folder. If you add
new examples or change any of the contracts, make sure that the examples
are still valid by running `make test/contracts/examples`.
