{-# language EmptyDataDeriving #-}
{-# language DeriveAnyClass #-}
{-# language DeriveFunctor #-}
{-# language DerivingStrategies #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}

module LamTreeB(
        VarAnn(..),
        LitAnn(..),
        Extra(..),
        LamAST(..),
        AppArgAST, AppFunAST) where

import Bound
import Control.Monad.Trans.Identity
import Data.Pointed
import Data.Functor.Classes
import Data.Void

-- all of these kind annotations are needed because backpack doesn't accept
-- extra polymorphism in the structure over what's in the signature.

type VarAnn = ()

type LitAnn = ()

newtype LamAST (lt :: * -> *) v = LamAST (Scope String lt v)
        deriving newtype Functor
        deriving newtype Applicative
        deriving newtype Monad
        deriving newtype Bound
        deriving newtype Show
        deriving anyclass Show1

data Extra (lt :: * -> *) (v :: *)
        deriving anyclass Functor
        deriving anyclass Applicative
        deriving anyclass Monad
        deriving anyclass Show

instance Show1 (Extra lt) where
        liftShowsPrec _ _ = showsPrec

type AppArgAST = IdentityT

type AppFunAST = IdentityT
