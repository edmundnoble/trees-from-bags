{-# language EmptyDataDeriving #-}
{-# language DeriveAnyClass #-}
{-# language DerivingStrategies #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language TypeFamilies #-}
{-# language PolyKinds #-}

module LamTreeN(
        VarAnn(..),
        LitAnn(..),
        Extra(..),
        LamAST(..),
        AppArgAST(..), AppFunAST(..)) where

import Control.Monad.Trans.Identity
import Data.Void

newtype VarAnn (lt :: * -> *) v = VarAnn () deriving newtype Show

newtype LamAST (lt :: * -> *) v = LamAST (lt v) deriving newtype Show

newtype LitAnn (lt :: * -> *) (v :: *) = LitAnn () deriving newtype Show

data Extra (lt :: * -> *) (v :: *) deriving Show

-- self-recursive!
newtype AppArgAST lt (v :: *) = AppArgAST (lt v) deriving newtype Show

newtype AppFunAST lt (v :: *) = AppFunAST (lt v) deriving newtype Show
