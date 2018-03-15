{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Formel
    ( Formel
    , konst
    , eval
    , parse
    ) where


import           Data.Maybe (fromMaybe)
import           Fix
import qualified Parser as P


data FormelF a r
    = Konstante a
    | BinOp Operand r r
    deriving (Eq, Functor)


type Formel a = Fix (FormelF a)


instance Eq a => Eq (Formel a) where
  Fix fa == Fix fb = fa == fb


data Operand
    = Add
    | Sub
    | Mul
    | Div
    deriving Eq


konst :: a -> Formel a
konst = Fix . Konstante


eval :: Fractional a => Formel a -> a
eval = cata evalI
  where
    evalI (Konstante n)  = n
    evalI (BinOp op a b) =
      let evaledOp = evalOp op
      in a `evaledOp` b

    evalOp Add = (+)
    evalOp Sub = (-)
    evalOp Mul = (*)
    evalOp Div = (/)



instance Num a => Num (Formel a) where
    fromInteger n = Fix $ Konstante (fromInteger n)
    a + b    = Fix $ BinOp Add a b
    a * b    = Fix $ BinOp Mul a b
    a - b    = Fix $ BinOp Sub a b
    abs      = undefined
    signum   = undefined


instance Fractional a => Fractional (Formel a) where
    a / b        = Fix $ BinOp Div a b
    fromRational = konst . fromRational


instance Show a => Show (Formel a) where
  showsPrec p formel = showString "Formel \"" . showsPrec' p formel . showString "\""
    where
      showsPrec' d f = cata showsPrecI f d
      showsPrecI (Konstante n)   = \d -> showParen (d > 10) $ showsPrec 0 n
      showsPrecI (BinOp Add a b) = \d -> showParen (d > 6)  $ a 6 . showString " + " . b 7
      showsPrecI (BinOp Sub a b) = \d -> showParen (d > 6)  $ a 6 . showString " - " . b 7
      showsPrecI (BinOp Mul a b) = \d -> showParen (d > 7)  $ a 7 . showString " * " . b 8
      showsPrecI (BinOp Div a b) = \d -> showParen (d > 7)  $ a 7 . showString " / " . b 8

---------------------------------------------------------------------
-- Formeln parsen

parse :: (Fractional a, Num a, Read a) => String -> Maybe (Formel a)
parse = P.parse formelP


formelP :: (Fractional a, Num a, Read a) => P.Parser (Formel a)
formelP = strichP


strichP :: (Fractional a, Num a, Read a) => Read a => P.Parser (Formel a)
strichP = P.chainl1 punktP opsP
  where opsP = P.oneOf $ map (uncurry opP) [ ('+',(+)), ('-',(-)) ]


punktP :: (Fractional a, Num a, Read a) => P.Parser (Formel a)
punktP = P.chainl1 konstP opsP
  where opsP = P.oneOf $ map (uncurry opP) [ ('*', (*)), ('/', (/)) ]


konstP :: (Fractional a, Num a, Read a) => P.Parser (Formel a)
konstP = P.oneOf
  [ P.between (chWs '(') (chWs ')') formelP
  , konst <$> numberP <* P.whitespace ]


opP :: Char -> a -> P.Parser a
opP c a = chWs c *> pure a


chWs :: Char -> P.Parser ()
chWs c = P.char (== c) *> P.whitespace


numberP :: (Fractional a, Read a) => P.Parser a
numberP =
  read <$> mconcat [vorzP, ziffernP, kommaP]
  where
    vorzP    = maybe "" return <$> P.try (P.char (== '-'))
    ziffernP = P.many1 P.digit
    kommaP   = fromMaybe "" <$> P.try (mconcat [pure <$> P.char (== '.'), P.many1 P.digit])

