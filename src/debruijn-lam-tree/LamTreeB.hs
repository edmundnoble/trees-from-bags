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

newtype VarAST (lt :: * -> *) v = VarAST v
        deriving Show

instance Show1 (VarAST lt) where
        liftShowsPrec sp _ p (VarAST v) = sp p v

newtype LamAST (lt :: * -> *) v = LamAST (Scope v lt String)
        deriving newtype Show
        deriving Show1

newtype LitAnn (lt :: * -> *) (v :: *) = LitAnn ()
        deriving newtype Show

instance Show1 (LitAnn lt) where
        liftShowsPrec _ _ _ _ = id

data Extra (lt :: * -> *) (v :: *)
        deriving Show

instance Show1 (Extra lt) where
        liftShowsPrec _ _ = showsPrec

-- self-recursive!
newtype AppArgAST lt (v :: *) = AppArgAST (lt v)
        deriving newtype Show

instance Show1 lt => Show1 (AppArgAST lt) where
        liftShowsPrec sp sl p (AppArgAST l)
                = liftShowsPrec sp sl p l

newtype AppFunAST lt (v :: *) = AppFunAST (lt v)
        deriving newtype Show

instance Show1 lt => Show1 (AppFunAST lt) where
        liftShowsPrec sp sl p (AppFunAST l)
                = liftShowsPrec sp sl p l
