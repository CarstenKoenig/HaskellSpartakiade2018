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

     describe "Parser sind Funktoren" $ do
       it "runParser (fmap (read . \\c -> [c]) digit) \"234\" liefert Int 2 und \"34\" als Rest" $
         runParser (fmap (read . \c -> [c]) digit) "234" `shouldBe` Just (2, "34")
       prop "fmap id ändert den Parser nicht" $ \s ->
         runParser (fmap id digit) s `shouldBe` runParser digit s
       prop "fmap bildet Kompositionen auf Kompositionen ab" $ \s ->
         runParser (fmap ((read :: String -> Int) . \c -> [c]) digit) s
         `shouldBe` runParser (fmap read . fmap (\c -> [c]) $ digit) s

     describe "der try - Kombinator" $ do
       it "liefert ein zustätzliches Just, falls der getestete Parser erfolgreich ist" $
         runParser (try digit) "123" `shouldBe` Just (Just '1', "23")
       it "soll aber mit Nothing erfolgreich sein und keine Eingabe konsumieren, falls der getestete Parser fehlschlägt" $
         runParser (try digit) "xy" `shouldBe` Just (Nothing, "xy")


     describe "Parser sind Appliatives" $ do
       it "runParser (pure (\\c1 c2 -> [c1,c2]) <*> digit <*> digit) \"234\" liefert \"12\" und \"3\" als Rest" $
         runParser (pure (\c1 c2 -> [c1,c2]) <*> digit <*> digit) "123" `shouldBe` Just ("12", "3")
       prop "erfüllt das identity Gesetz" $ \s ->
         runParser (pure id <*> digit) s `shouldBe` runParser digit s
       prop "erfüllt das Homomorphismus Gesetz" $ \s ->
         runParser (pure (+1) <*> pure 2) s `shouldBe` runParser (pure (2+1)) s
       prop "erfüllt das Interchange Gesetz" $ \s ->
         runParser (fmap (:) digit <*> pure "x") s `shouldBe` runParser (pure ($ "x") <*> fmap (:) digit) s
       prop "erfüllt das Composition Gesetz" $ \s ->
         runParser (fmap (\a cont -> cont a) digit <*> (fmap (\b c a -> [a,b,c]) digit <*> digit)) s
         `shouldBe` runParser (pure (.) <*> fmap (\a cont -> cont a) digit <*> fmap (\b c a -> [a,b,c]) digit <*> digit) s
