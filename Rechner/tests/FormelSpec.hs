{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FormelSpec (main, spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Modifiers

import Formel

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  describe "Wie funktioniert das hier?" $ do
    it "Ein erfolgreicher Test" $
      4 + 5 `shouldBe` 9
    it "Ein fehlschlagender Test" $
      4 + 5 `shouldBe` 10
    prop "Ein eigenschaftsbasierender Test" $ \ (n :: Int) ->
      2 * n `shouldBe` n + n

  where
    shouldApproxBe a b = abs (a - b) < 0.0001 
