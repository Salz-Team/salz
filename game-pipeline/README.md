# Game Pipeline

The game pipeline is responsible for match-making, match-execution, and uploading match results.

## Getting Started

So far, we have the match-handler, a tic tac toe game-engine, and a tic tac toe sample player bot.

You can run a tic tac toe game between two of the sample bots as follows:

```sh
cabal run match-handler -- -g "cabal run tic-tac-toe" -b "cabal run tic-tac-toe-player" -b "cabal run tic-tac-toe-player"
```
