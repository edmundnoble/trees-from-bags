{-# language DeriveAnyClass #-}
{-# language DeriveFunctor #-}
{-# language DerivingStrategies #-}
{-# language ExplicitForAll #-}
{-# language FlexibleContexts #-}
{-# language InstanceSigs #-}
-- I think having to enable this is a bug.
{-# language MonoLocalBinds #-}
{-# language QuantifiedConstraints #-}
{-# language StandaloneDeriving #-}
{-# language UndecidableInstances #-}

module LamTree(LamTree(..)) where

import Control.Monad(ap)
import Data.Default
import Data.Functor.Classes
import Bound

import LamTreeS

data LamTree v
        = AppR (AppFunAST LamTree v) (AppArgAST LamTree v)
        | VarR v VarAnn
        | LamR (LamAST LamTree v)
        | LitR Int LitAnn
        | ExtraR (Extra LamTree v)

-- woohoo, recursion!
deriving stock instance (
        Show (AppFunAST LamTree v),
        Show (AppArgAST LamTree v),
        Show (LamAST LamTree v),
        Show LitAnn,
        Show VarAnn,
        Show (Extra LamTree v),
        Show v
        ) =>
        Show (LamTree v)

-- woohoo, recursion!
instance (
        Show1 (AppFunAST LamTree),
        Show1 (AppArgAST LamTree),
        Show1 (LamAST LamTree),
        Show LitAnn,
        Show VarAnn,
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
                                showsPrec p ann
        liftShowsPrec sp sl p (LamR l) =
                showsPrec p "LamR" .
                        liftShowsPrec sp sl p l
        liftShowsPrec sp sl p (LitR i ann) =
                showsPrec p "LamR" .
                        showsPrec p i .
                                showsPrec p ann
        liftShowsPrec sp sl p (ExtraR e) =
                showsPrec p "ExtraR" .
                        liftShowsPrec sp sl p e

deriving stock instance (
        Functor (AppArgAST LamTree),
        Functor (AppFunAST LamTree),
        Functor (LamAST LamTree),
        Functor (Extra LamTree)) => Functor LamTree

instance (
        Functor (AppArgAST LamTree),
        Functor (AppFunAST LamTree),
        Functor (LamAST LamTree),
        Default LitAnn,
        Default VarAnn,
        Functor (Extra LamTree),
        Bound AppArgAST,
        Bound AppFunAST,
        Bound LamAST,
        Bound Extra) => Applicative LamTree where
        pure = return
        (<*>) = ap

instance (
        Functor (AppArgAST LamTree),
        Functor (AppFunAST LamTree),
        Functor (LamAST LamTree),
        Default LitAnn,
        Default VarAnn,
        Functor (Extra LamTree),
        Bound AppArgAST,
        Bound AppFunAST,
        Bound LamAST,
        Bound Extra) => Monad LamTree where
        return v = VarR v def
        (AppR arg fun) >>= f
                = AppR
                (arg >>>= f)
                (fun >>>= f)
        (LamR a) >>= f
                = LamR
                (a >>>= f)
        (VarR v _) >>= f = f v
        (LitR i _) >>= f = LitR i def
        (ExtraR a) >>= f = ExtraR (a >>>= f)
