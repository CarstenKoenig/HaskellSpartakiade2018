{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parser
  ( Parser
  , parse
  , runParser
  , succeed
  , Parser.fail
  , one
  , digit
  , char
  , try
  ) where

import Control.Applicative (Alternative (..))
import Data.Char (isDigit)
import Data.Semigroup (Semigroup (..), (<>))


parse :: Parser a -> String -> Maybe a
parse pa = fmap fst . runParser pa


newtype Parser a
  = Parser
  { runParser :: String -> Maybe (a, String)
  }


succeed :: a -> Parser a
succeed x = Parser $ \s -> Just (x, s)


fail :: Parser a
fail = Parser $ const Nothing


one :: Parser Char
one = char (const True)


digit :: Parser Char
digit = char isDigit


char :: (Char -> Bool) -> Parser Char
char praed = Parser $ \case
  (c:s) | praed c -> Just (c, s)
  _               -> Nothing


instance Functor Parser where
  fmap f p =
    Parser $ fmap (\(x, rest) -> (f x, rest)) . runParser p


try :: Parser a -> Parser (Maybe a)
try pa = Parser $ \s ->
  case runParser pa s of
    Nothing      -> Just (Nothing, s)
    Just (a, s') -> Just (Just a, s')


instance Applicative Parser where
  pure = succeed
  pf <*> pa = Parser $ \s -> do
    (f, s') <- runParser pf s
    (x, s'') <- runParser pa s'
    return (f x, s'')


