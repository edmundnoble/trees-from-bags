{-# language DeriveAnyClass #-}
{-# language DeriveFunctor #-}
{-# language DerivingStrategies #-}
{-# language ExplicitForAll #-}
{-# language FlexibleContexts #-}
{-# language InstanceSigs #-}
-- I think having to enable this is a bug.
{-# language MonoLocalBinds #-}
{-# language StandaloneDeriving #-}
{-# language UndecidableInstances #-}

module LamTree(LamTree(..)) where

import Control.Monad(ap)
import Data.Functor.Classes
import Bound

import LamTreeS

data LamTree v
        = AppR (AppFunAST LamTree v) (AppArgAST LamTree v)
        | VarR v (VarAnn LamTree v)
        | LamR (LamAST LamTree v)
        | LitR Int (LitAnn LamTree v)
        | ExtraR (Extra LamTree v)

-- woohoo, recursion!
deriving instance (
        Show (AppFunAST LamTree v),
        Show (AppArgAST LamTree v),
        Show (LamAST LamTree v),
        Show (LitAnn LamTree v),
        Show (VarAnn LamTree v),
        Show (Extra LamTree v),
        Show v
        ) =>
        Show (LamTree v)

-- woohoo, recursion!
instance (
        Show1 (AppFunAST LamTree),
        Show1 (AppArgAST LamTree),
        Show1 (LamAST LamTree),
        Show1 (LitAnn LamTree),
        Show1 (VarAnn LamTree),
        Show1 (Extra LamTree)
        ) =>
        Show1 LamTree where
        liftShowsPrec sp sl p (AppR f l) =
                showsPrec p "AppR" .
                        liftShowsPrec sp sl p f .
                                liftShowsPrec sp sl p l
        liftShowsPrec sp sl p (VarR v ann) =
                showsPrec p "VarR" .
                        sp p v .
                                liftShowsPrec sp sl p ann
        liftShowsPrec sp sl p (LamR l) =
                showsPrec p "LamR" .
                        liftShowsPrec sp sl p l
        liftShowsPrec sp sl p (LitR i l) =
                showsPrec p "LamR" .
                        showsPrec p i .
                        liftShowsPrec sp sl p l
        liftShowsPrec sp sl p (ExtraR e) =
                showsPrec p "ExtraR" .
                        liftShowsPrec sp sl p e

deriving instance (
        Functor (AppArgAST LamTree),
        Functor (AppFunAST LamTree),
        Functor (LamAST LamTree),
        Functor (LitAnn LamTree),
        Functor (VarAnn LamTree),
        Functor (Extra LamTree)) => Functor LamTree

instance (
        Monad (AppArgAST LamTree),
        Monad (AppFunAST LamTree),
        Monad (LamAST LamTree),
        Monad (LitAnn LamTree),
        Monad (VarAnn LamTree),
        Monad (Extra LamTree),
        Bound AppArgAST,
        Bound AppFunAST,
        Bound LamAST,
        Bound LitAnn,
        Bound Extra) => Applicative LamTree where
        pure = return
        (<*>) = ap

instance (
        Monad (AppArgAST LamTree),
        Monad (AppFunAST LamTree),
        Monad (LamAST LamTree),
        Monad (LitAnn LamTree),
        Monad (VarAnn LamTree),
        Monad (Extra LamTree),
        Bound AppArgAST,
        Bound AppFunAST,
        Bound LamAST,
        Bound LitAnn,
        Bound Extra) => Monad LamTree where
        return v = VarR v (return v)
        (AppR arg fun) >>= f
                = AppR
                (arg >>>= f)
                (fun >>>= f)
        (LamR a) >>= f
                = LamR
                (a >>>= f)
        (VarR v _) >>= f = f v
        (LitR i ann) >>= f = LitR i (ann >>>= f)
        (ExtraR a) >>= f = ExtraR (a >>>= f)
