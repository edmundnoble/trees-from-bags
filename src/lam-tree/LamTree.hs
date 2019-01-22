{-# language DeriveAnyClass #-}
{-# language ExplicitForAll #-}
{-# language FlexibleContexts #-}
{-# language InstanceSigs #-}
-- I think having to enable this is a bug.
{-# language MonoLocalBinds #-}
{-# language StandaloneDeriving #-}
{-# language UndecidableInstances #-}

module LamTree(LamTree(..)) where

import Data.Functor.Classes

import LamTreeG

data LamTree v
        = AppR (AppFunAST LamTree v) (AppArgAST LamTree v)
        | VarR (VarAST LamTree v)
        | LamR (LamAST LamTree v)
        | LitR Int (LitAnn LamTree v)
        | ExtraR (Extra LamTree v)

-- woohoo, recursion!
deriving instance (
        Show (AppFunAST LamTree v),
        Show (AppArgAST LamTree v),
        Show (LamAST LamTree v),
        Show (LitAnn LamTree v),
        Show (VarAST LamTree v),
        Show (Extra LamTree v)
        ) =>
        Show (LamTree v)

-- woohoo, recursion!
instance (
        Show1 (AppFunAST LamTree),
        Show1 (AppArgAST LamTree),
        Show1 (LamAST LamTree),
        Show1 (LitAnn LamTree),
        Show1 (VarAST LamTree),
        Show1 (Extra LamTree)
        ) =>
        Show1 LamTree where
        liftShowsPrec sp sl p (AppR f l) =
                showsPrec p "AppR" .
                        liftShowsPrec sp sl p f .
                                liftShowsPrec sp sl p l
        liftShowsPrec sp sl p (VarR v) =
                showsPrec p "VarR" .
                        liftShowsPrec sp sl p v
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
