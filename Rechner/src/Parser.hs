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
  , oneOf
  , many
  , many1
  , chainl
  , chainl1
  , between
  , whitespace
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


instance Alternative Parser where
  empty     = Parser.fail
  p1 <|> p2 = Parser $ \s ->
    case runParser p1 s of
      ok@(Just _) -> ok
      Nothing     -> runParser p2 s


many1 :: Parser a -> Parser [a]
many1 = some


oneOf :: [Parser a] -> Parser a
oneOf = foldr (<|>) empty


instance Monoid a => Monoid (Parser a) where
  mempty          = succeed mempty
  p1 `mappend` p2 = mappend <$> p1 <*> p2


instance Monad Parser where
  return     = pure
  pa >>= fpb = Parser $ \s -> do
    (a, s') <- runParser pa s
    runParser (fpb a) s'


between :: Parser l -> Parser r -> Parser a -> Parser a
between pl pr pa = undefined


whitespace :: Parser ()
whitespace = undefined


chainl1 :: forall a . Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 pa pop = undefined


chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl pa pop va = undefined
