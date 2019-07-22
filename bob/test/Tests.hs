{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec        (describe )
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import qualified BobSpecs
import qualified SimpleBobSpecs

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} $ do
  describe "Bob" BobSpecs.specs
  describe "SimpleBob" SimpleBobSpecs.specs
