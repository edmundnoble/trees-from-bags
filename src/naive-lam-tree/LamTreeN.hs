{-# language TypeFamilies #-}
{-# language PolyKinds #-}

module LamTreeN(
        VarAST(..),
        LitAnn(..),
        Extra(..),
        LamAST(..),
        AppArgAST(..), AppFunAST(..)) where

import Control.Monad.Trans.Identity
import Data.Void

import Common

data Incr a = Z | S a
newtype Scope f a = Scope (f (Incr a))

newtype VarAST v (lt :: * -> *) = VarAST v
newtype LamAST v (lt :: * -> *) = LamAST (lt v)
newtype LitAnn (v :: *) (lt :: * -> *) = LitAnn ()
-- we could also leave this uninstantiated; we won't be able to write instances
-- that touch it, but later if we want to assign to LamTreeX the rest of our
-- code will still work!
data Extra (v :: *) (lt :: * -> *)
-- self-recursive!
newtype AppArgAST (v :: *) lt = AppArgAST (lt v)
newtype AppFunAST (v :: *) lt = AppFunAST (lt v)
