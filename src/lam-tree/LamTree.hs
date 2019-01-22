module LamTree(LamTree(..)) where

import LamTreeG

data LamTree a
        = App a a (AppA a)
        | Lam a a (LamA a)
        | Lit Int LitA
