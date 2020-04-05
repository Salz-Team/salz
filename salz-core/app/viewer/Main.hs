{-# LANGUAGE DataKinds, OverloadedStrings #-}
module Main where

import Viewer
import ViewerOptions

main :: IO ()
main = parseInput >>= startViewer
