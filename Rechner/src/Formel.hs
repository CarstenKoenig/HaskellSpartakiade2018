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
    | Mult
    | Div
    deriving Eq


konst :: a -> Formel a
konst = undefined


eval :: Fractional a => Formel a -> a
eval = undefined


----------------------------------------------------------------------
-- Formeln parsen

parse :: (Fractional a, Num a, Read a) => String -> Maybe (Formel a)
parse = undefined
