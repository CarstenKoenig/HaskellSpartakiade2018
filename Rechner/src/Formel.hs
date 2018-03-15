module Formel
    ( Formel
    , konst
    , eval
    , parse
    ) where


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

----------------------------------------------------------------------
-- Formeln parsen

parse :: (Fractional a, Num a, Read a) => String -> Maybe (Formel a)
parse = undefined
