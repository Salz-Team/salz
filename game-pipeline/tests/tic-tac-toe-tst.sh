echo '{ "messageType": "gameStart", "numberOfPlayers": 2 }
{ "messageType": "playerResponses", "responses": [{"player":0, "isValid": true, "response": { "move": [0, 0]}}]}
{ "messageType": "playerResponses", "responses": [{"player":1, "isValid": true, "response": { "move": [1, 0]}}]}
{ "messageType": "playerResponses", "responses": [{"player":0, "isValid": true, "response": { "move": [2, 0]}}]}
{ "messageType": "playerResponses", "responses": [{"player":1, "isValid": true, "response": { "move": [1, 1]}}]}
{ "messageType": "playerResponses", "responses": [{"player":0, "isValid": true, "response": { "move": [2, 2]}}]}
{ "messageType": "playerResponses", "responses": [{"player":1, "isValid": true, "response": { "move": [1, 2]}}]}' | cabal run tic-tac-toe

cabal run game-broker -- -g "dist-newstyle/build/x86_64-linux/ghc-9.6.6/game-pipeline-0.1.0.0/x/tic-tac-toe/build/tic-tac-toe/tic-tac-toe" -b "dist-newstyle/build/x86_64-linux/ghc-9.6.6/game-pipeline-0.1.0.0/x/tic-tac-toe-player/build/tic-tac-toe-player/tic-tac-toe-player" -b "dist-newstyle/build/x86_64-linux/ghc-9.6.6/game-pipeline-0.1.0.0/x/tic-tac-toe-player/build/tic-tac-toe-player/tic-tac-toe-player"

