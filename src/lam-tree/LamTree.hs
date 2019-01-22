{-# language FlexibleContexts #-}
-- I think having to enable this is a bug.
{-# language MonoLocalBinds #-}
{-# language StandaloneDeriving #-}
{-# language UndecidableInstances #-}

module LamTree(LamTree(..)) where

import LamTreeG

data LamTree v
        = AppR (AppFunAST v LamTree) (AppArgAST v LamTree)
        | VarR (VarAST v LamTree)
        | LamR (LamAST v LamTree)
        | LitR Int (LitAnn v LamTree)
        | ExtraR (Extra v LamTree)

-- woohoo, recursion!
deriving instance (
        Show (AppFunAST v LamTree),
        Show (AppArgAST v LamTree),
        Show (LamAST v LamTree),
        Show (LitAnn v LamTree),
        Show (VarAST v LamTree),
        Show (Extra v LamTree)
        ) =>
        Show (LamTree v)
