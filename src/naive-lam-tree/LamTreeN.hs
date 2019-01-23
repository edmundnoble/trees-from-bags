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

import Control.Monad.Trans.Identity
import Data.Void

-- all of these kind annotations are needed because backpack doesn't accept
-- extra polymorphism in the structure over what's in the signature.

type VarAnn = ()

type LitAnn = ()

newtype LamAST (lt :: * -> *) v = LamAST (lt v) deriving newtype Show

data Extra (lt :: * -> *) (v :: *) deriving anyclass Show

newtype AppArgAST lt (v :: *) = AppArgAST (lt v) deriving newtype Show

newtype AppFunAST lt (v :: *) = AppFunAST (lt v) deriving newtype Show
