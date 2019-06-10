module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Control.Monad.Eff (Eff)

main :: Effect Unit
main = do
  signal <- input
  game <- foldp step start signal
  runSignal (map render game)
  log "Hello sailor!"
