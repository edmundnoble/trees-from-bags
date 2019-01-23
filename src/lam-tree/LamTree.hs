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
        = AppR !(AppFunAST LamTree v) !(AppArgAST LamTree v)
        | VarR {-# unpack #-} !v {-# unpack #-} !VarAnn
        | LamR !(LamAST LamTree v)
        | LitR {-# unpack #-} !Int {-# unpack #-} !LitAnn
        | ExtraR !(Extra LamTree v)

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
                ("AppR " ++) .
                liftShowsPrec sp sl p f .
                (" " ++) .
                liftShowsPrec sp sl p l
        liftShowsPrec sp sl p (VarR v ann) =
                ("VarR " ++) .
                sp p v .
                (" " ++) .
                showsPrec p ann
        liftShowsPrec sp sl p (LamR l) =
                ("LamR " ++) .
                liftShowsPrec sp sl p l
        liftShowsPrec sp sl p (LitR i ann) =
                ("LitR " ++) .
                showsPrec p i .
                (" " ++) .
                showsPrec p ann
        liftShowsPrec sp sl p (ExtraR e) =
                ("ExtraR " ++) .
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
        (LitR i a) >>= _ = LitR i a
        (ExtraR a) >>= f = ExtraR (a >>>= f)
