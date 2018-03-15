module Formel
    ( Formel
    , konst
    , eval
    , parse
    ) where


import           Data.Maybe (fromMaybe)
import qualified Parser as P


data Formel a
    = Konstante a
    | BinOp Operand (Formel a) (Formel a)
    deriving Eq


data Operand
    = Add
    | Sub
    | Mul
    | Div
    deriving Eq


konst :: a -> Formel a
konst = Konstante


eval :: Fractional a => Formel a -> a
eval (Konstante n)  = n
eval (BinOp op a b) =
  let evaledOp = evalOp op
  in eval a `evaledOp` eval b

  where
    evalOp Add = (+)
    evalOp Sub = (-)
    evalOp Mul = (*)
    evalOp Div = (/)



instance Num a => Num (Formel a) where
    fromInteger n = Konstante (fromInteger n)
    a + b    = BinOp Add a b
    a * b    = BinOp Mul a b
    a - b    = BinOp Sub a b
    abs      = undefined
    signum   = undefined


instance Fractional a => Fractional (Formel a) where
    a / b        = BinOp Div a b
    fromRational = Konstante . fromRational


instance Show a => Show (Formel a) where
  showsPrec p formel = showString "Formel \"" . showsPrec' p formel . showString "\""
    where
      showsPrec' d (Konstante n)   = showParen (d > 10) $ showsPrec 0 n
      showsPrec' d (BinOp Add a b) = showParen (d > 6)  $ showsPrec' 6 a . showString " + " . showsPrec' 7 b
      showsPrec' d (BinOp Sub a b) = showParen (d > 6)  $ showsPrec' 6 a . showString " - " . showsPrec' 7 b
      showsPrec' d (BinOp Mul a b) = showParen (d > 7)  $ showsPrec' 7 a . showString " * " . showsPrec' 8 b
      showsPrec' d (BinOp Div a b) = showParen (d > 7)  $ showsPrec' 7 a . showString " / " . showsPrec' 8 b

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
  , Konstante <$> numberP <* P.whitespace ]


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

