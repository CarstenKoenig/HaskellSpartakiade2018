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

   describe "Beim Parsen von Formeln" $ do
     it "werden Konstanten erkannt" $
       parse "42" `shouldBe` Just (konst 42)
     it "Addition wird erkannt" $
       parse "11+13" `shouldBe` Just (konst 11 + konst 13)
     it "Addition-Kette wird erkannt" $
       parse "1+2+3" `shouldBe` Just (konst 1 + konst 2 + konst 3)
     it "Subtraktion-Kette wird erkannt" $
       parse "1-2-3" `shouldBe` Just (konst 1 - konst 2 - konst 3)
     it "Multiplikation-Kette wird erkannt" $
       parse "1*2*3" `shouldBe` Just (konst 1 * konst 2 * konst 3)
     it "Division-Kette wird erkannt" $
       parse "1/2/3" `shouldBe` Just (konst 1 / konst 2 / konst 3)
     it "Punkt vor Strich wird erkannt" $
       parse "1*2+3" `shouldBe` Just ((konst 1 * konst 2) + konst 3)
     it "Klammern werden erkannt" $
       parse "1*(2+3)" `shouldBe` Just (konst 1 * (konst 2 + konst 3))
     it "Whitespace ist kein Problem" $
       parse "1  *( 2+ 3 )" `shouldBe` Just (konst 1 * (konst 2 + konst 3))


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
       show (konst n) `shouldBe` mitFormel (show n)
     prop "werden Additionen mit + angezeigt" $ \ (a :: Double) (b :: Double) ->
       show (konst a + konst b) `shouldBe` mitFormel (show a ++ " + " ++ show b)
     prop "werden Subtraktionen mit - angezeigt" $ \ (a :: Double) (b :: Double) ->
       show (konst a - konst b) `shouldBe` mitFormel (show a ++ " - " ++ show b)
     prop "werden Multiplikationen mit * angezeigt" $ \ (a :: Double) (b :: Double) ->
       show (konst a * konst b) `shouldBe` mitFormel (show a ++ " * " ++ show b)
     prop "werden Divisionen mit / angezeigt" $ \ (a :: Double) (b :: Double) ->
       show (konst a / konst b) `shouldBe` mitFormel (show a ++ " / " ++ show b)
     prop "keine Klammern wegen Punkt vor Strich" $ \ (a :: Double) (b :: Double) (c :: Double) ->
       show (konst a * konst b + konst c) `shouldBe` mitFormel (show a ++ " * " ++ show b ++ " + " ++ show c)
     prop "keine Klammern bei Mult/Add" $ \ (a :: Double) (b :: Double) (c :: Double) ->
       show (konst a * (konst b + konst c)) `shouldBe` mitFormel (show a ++ " * (" ++ show b ++ " + " ++ show c ++ ")")
     prop "Subtraktion wird links-assoziativ ausgegeben" $ \ (a :: Double) (b :: Double) (c :: Double) ->
       show (konst a - konst b - konst c) `shouldBe` mitFormel (show a ++ " - " ++ show b ++ " - " ++ show c)
     prop "Subtraktion wird links-assoziativ geklammert" $ \ (a :: Double) (b :: Double) (c :: Double) ->
       show (konst a - (konst b - konst c)) `shouldBe` mitFormel (show a ++ " - (" ++ show b ++ " - " ++ show c ++ ")")
     prop "Division wird links-assoziativ ausgegeben" $ \ (a :: Double) (b :: Double) (c :: Double) ->
       show (konst a / konst b / konst c) `shouldBe` mitFormel (show a ++ " / " ++ show b ++ " / " ++ show c)
     prop "Division wird links-assoziativ geklammert" $ \ (a :: Double) (b :: Double) (c :: Double) ->
       show (konst a / (konst b / konst c)) `shouldBe` mitFormel (show a ++ " / (" ++ show b ++ " / " ++ show c ++ ")")
   where
     mitFormel s = "Formel \"" ++ s ++ "\""

shouldApproxBe :: Double -> Double -> Bool
shouldApproxBe a b = abs (a - b) < 0.0001 
