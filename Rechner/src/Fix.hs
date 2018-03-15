module Fix where

newtype Fix f =
  Fix { unFix :: f (Fix f) }


type Algebra f a = f a -> a


cata :: Functor f => Algebra f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix
