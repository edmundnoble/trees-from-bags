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
import Data.Functor.Const
import Data.Void

import Common

newtype VarAST v (lt :: * -> *) = VarAST v
newtype LamAST v (lt :: * -> *) = LamAST (Scope v lt String)
newtype LitAnn (v :: *) (lt :: * -> *) = LitAnn ()
data Extra (v :: *) (lt :: * -> *)
-- self-recursive!
newtype AppArgAST (v :: *) lt = AppArgAST (lt v)
newtype AppFunAST (v :: *) lt = AppFunAST (lt v)
