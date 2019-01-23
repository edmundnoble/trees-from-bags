{-# language EmptyDataDeriving #-}
{-# language DeriveAnyClass #-}
{-# language DeriveFunctor #-}
{-# language DerivingStrategies #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}

module LamTreeN(
        VarAnn(..),
        LitAnn(..),
        Extra(..),
        LamAST(..),
        AppArgAST(..), AppFunAST(..)) where

import Bound
import Control.Monad.Trans.Class
import Control.Monad.Trans.Identity
import Data.Functor.Classes

-- all of these kind annotations are needed because backpack doesn't accept
-- extra polymorphism in the structure over what's in the signature.

type VarAnn = ()

type LitAnn = ()

type LamAST = IdentityT

data Extra (lt :: * -> *) (v :: *)
        deriving anyclass Functor
        deriving anyclass Applicative
        deriving anyclass Monad
        deriving anyclass Show

instance MonadTrans Extra where
instance Bound Extra where

instance Show1 (Extra lt) where
        liftShowsPrec _ _ = showsPrec

type AppArgAST = IdentityT

type AppFunAST = IdentityT
