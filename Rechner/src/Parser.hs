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
succeed x = undefined


fail :: Parser a
fail = undefined


one :: Parser Char
one = undefined


digit :: Parser Char
digit = undefined
