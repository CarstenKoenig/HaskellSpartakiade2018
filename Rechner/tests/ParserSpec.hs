{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ParserSpec (main, spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Modifiers

import Parser

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec


spec :: Spec
spec = do

   describe "Parser Tests" $ do
     it "runParser (succeed True) \"333\" liefert True und konsumiert nichts von der Eingabe" $
       runParser (succeed True) "333" `shouldBe` Just (True, "333")

     it "runParser fail \"333\" liefert Nothing" $
       runParser (Parser.fail :: Parser Bool) "333" `shouldBe` Nothing

     it "runParser one \"123\" liefert Just ('1', \"23\")" $
       runParser one "123" `shouldBe` Just ('1', "23")

     it "runParser digit \"123\" liefert Just ('1', \"23\")" $
       runParser digit "123" `shouldBe` Just ('1', "23")

     it "runParser digit \"xy\" liefert Nothing" $
       runParser digit "xy" `shouldBe` Nothing
