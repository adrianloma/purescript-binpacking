module Main where

import Prelude
import Data.BinPacking (Item)

import Effect (Effect)
import Effect.Console (log)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

newtype MyRecord = MyRecord { a :: Int }

derive instance genericMyRecord :: Generic MyRecord _

instance showMyRecord :: Show MyRecord where
  show = genericShow

main :: Effect Unit
main = do
  log $ show (MyRecord {a: 3})

x :: Item -> String
x = const "3"

