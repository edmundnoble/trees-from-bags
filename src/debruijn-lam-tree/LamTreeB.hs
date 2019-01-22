{-# language EmptyDataDeriving #-}
{-# language DerivingStrategies #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language TypeFamilies #-}
{-# language PolyKinds #-}

module LamTreeB(
        VarAST(..),
        LitAnn(..),
        Extra(..),
        LamAST(..),
        AppArgAST(..), AppFunAST(..)) where

import Bound.Scope
import Control.Monad.Trans.Identity
import Data.Void

import Common

newtype VarAST v (lt :: * -> *) = VarAST v deriving newtype Show
newtype LamAST v (lt :: * -> *) = LamAST (Scope v lt String) deriving newtype Show
newtype LitAnn (v :: *) (lt :: * -> *) = LitAnn () deriving newtype Show
data Extra (v :: *) (lt :: * -> *) deriving Show
-- self-recursive!
newtype AppArgAST (v :: *) lt = AppArgAST (lt v) deriving newtype Show
newtype AppFunAST (v :: *) lt = AppFunAST (lt v) deriving newtype Show
