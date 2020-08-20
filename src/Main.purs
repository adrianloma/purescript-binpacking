module Main where

import Prelude
import Data.BinPacking.Item

import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log "🍝"

x :: Item -> String
x = const "3"
