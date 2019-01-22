{-# language PolyKinds #-}
{-# language GADTs #-}
{-# language KindSignatures #-}

module Common(Const3(..), Swap(..)) where

-- not a monad transformer.
newtype Const3 (a :: *) (b :: k1) (c :: k2)
        = Const3 { runConst3 :: a }

newtype Swap (f :: k1 -> k2 -> *) (b :: k2) (a :: k1) where
        Swap :: f a b -> Swap f b a
