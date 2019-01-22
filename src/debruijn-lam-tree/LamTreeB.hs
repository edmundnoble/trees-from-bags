{-# language EmptyDataDeriving #-}
{-# language DeriveAnyClass #-}
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
import Data.Functor.Classes
import Data.Void

import Common

newtype VarAST (lt :: * -> *) v = VarAST v
        deriving Show

instance Show1 lt => Show1 (VarAST lt) where
        liftShowsPrec sp sl p (VarAST v) = sp p v

newtype LamAST (lt :: * -> *) v = LamAST (Scope v lt String)
        deriving newtype Show
        deriving Show1

-- doesn't work yet
-- instance Show1 lt => Show1 (LamAST lt) where
--         liftShowsPrec sp sl p (LamAST v) = liftShowsPrec showsPrec showList p v

newtype LitAnn (lt :: * -> *) (v :: *) = LitAnn ()
        deriving newtype Show
        deriving Show1

data Extra (lt :: * -> *) (v :: *)
        deriving (Show, Show1)
-- self-recursive!
newtype AppArgAST lt (v :: *) = AppArgAST (lt v)
        deriving newtype Show
        deriving Show1
newtype AppFunAST lt (v :: *) = AppFunAST (lt v)
        deriving newtype Show
        deriving Show1
