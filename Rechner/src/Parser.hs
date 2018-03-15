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
one = Parser $ \case
  (c:rest) -> Just (c, rest)
  _        -> Nothing


digit :: Parser Char
digit = Parser $ \case
  (c:rest) | isDigit c -> Just (c, rest)
  _                    -> Nothing

