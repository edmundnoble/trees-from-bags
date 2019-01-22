{-# language TypeFamilies #-}

module LamTreeC(LamA, AppA, LitA) where

type family LamA a
type instance LamA a = String
type family AppA a
type instance AppA a = (String, Int)
type family LitA
type instance LitA = ()
