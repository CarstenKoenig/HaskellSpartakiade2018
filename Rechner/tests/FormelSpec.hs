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
   describe "Beim Auswerten von Formel" $ do
     prop "werden Konstanten auf ihren Wert berechnet" $ \ (n :: Double) ->
       eval (konst n) `shouldApproxBe` n
     prop "werden Additionen zweier Konstanten korrekt berechnet" $ \ (a :: Double) (b :: Double) ->
       eval (konst a + konst b) `shouldApproxBe` (a + b)
     prop "werden Subtraktionen zweier Konstanten korrekt berechnet" $ \ (a :: Double) (b :: Double) ->
       eval (konst a - konst b) `shouldApproxBe` (a - b)
     prop "werden Multiplikationen zweier Konstanten korrekt berechnet" $ \ (a :: Double) (b :: Double) ->
       eval (konst a * konst b) `shouldApproxBe` (a * b)
     prop "werden Divisionen zweier Konstanten korrekt berechnet" $ \ (a :: Double) (NonZero b :: NonZero Double) ->
       eval (konst a / konst b) `shouldApproxBe` (a / b)
     prop "ist die Subtraktion links-assoziativ" $ \ (a :: Double) (b :: Double) (c :: Double) ->
       eval (konst a - konst b - konst c) `shouldApproxBe` ((a - b) - c)
     prop "ist die Division links-assoziativ" $ \ (a :: Double) (NonZero b :: NonZero Double) (NonZero c :: NonZero Double) ->
       eval (konst a / konst b / konst c) `shouldApproxBe` ((a / b) / c)
     prop "gilt Punkt-Vor-Strich" $ \ (a :: Double) (b :: Double) (c :: Double) ->
       eval (konst a * konst b + konst c) `shouldApproxBe` (a * b + c)
     prop "gilt das Distributivgesetz" $ \ (a :: Double) (b :: Double) (c :: Double) ->
       eval (konst a * (konst b + konst c)) `shouldApproxBe` (a * b + a * c)


   describe "Beim `show` von Formeln" $ do
     prop "werden Konstanten direkt ausgegeben" $ \ (n :: Double) ->
       show (konst n) `shouldBe` show n
     prop "werden Additionen mit + angezeigt" $ \ (a :: Double) (b :: Double) ->
       show (konst a + konst b) `shouldBe` show a ++ " + " ++ show b
     prop "werden Subtraktionen mit - angezeigt" $ \ (a :: Double) (b :: Double) ->
       show (konst a - konst b) `shouldBe` show a ++ " - " ++ show b
     prop "werden Multiplikationen mit * angezeigt" $ \ (a :: Double) (b :: Double) ->
       show (konst a * konst b) `shouldBe` show a ++ " * " ++ show b
     prop "werden Divisionen mit / angezeigt" $ \ (a :: Double) (b :: Double) ->
       show (konst a / konst b) `shouldBe` show a ++ " / " ++ show b
     prop "keine Klammern wegen Punkt vor Strich" $ \ (a :: Double) (b :: Double) (c :: Double) ->
       show (konst a * konst b + konst c) `shouldBe` show a ++ " * " ++ show b ++ " + " ++ show c
     prop "keine Klammern bei Mult/Add" $ \ (a :: Double) (b :: Double) (c :: Double) ->
       show (konst a * (konst b + konst c)) `shouldBe` show a ++ " * (" ++ show b ++ " + " ++ show c ++ ")"
     prop "Subtraktion wird links-assoziativ ausgegeben" $ \ (a :: Double) (b :: Double) (c :: Double) ->
       show (konst a - konst b - konst c) `shouldBe` show a ++ " - " ++ show b ++ " - " ++ show c
     prop "Subtraktion wird links-assoziativ geklammert" $ \ (a :: Double) (b :: Double) (c :: Double) ->
       show (konst a - (konst b - konst c)) `shouldBe` show a ++ " - (" ++ show b ++ " - " ++ show c ++ ")"
     prop "Division wird links-assoziativ ausgegeben" $ \ (a :: Double) (b :: Double) (c :: Double) ->
       show (konst a / konst b / konst c) `shouldBe` show a ++ " / " ++ show b ++ " / " ++ show c
     prop "Division wird links-assoziativ geklammert" $ \ (a :: Double) (b :: Double) (c :: Double) ->
       show (konst a / (konst b / konst c)) `shouldBe` show a ++ " / (" ++ show b ++ " / " ++ show c ++ ")"


shouldApproxBe :: Double -> Double -> Bool
shouldApproxBe a b = abs (a - b) < 0.0001 
