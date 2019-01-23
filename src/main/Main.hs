{-# language PatternSynonyms #-}
{-# language UndecidableInstances #-}

module Main where

import Control.Monad.Trans.Identity
import Data.Functor.Const
import Data.Functor.Identity
import qualified InstantiatedDeBruijn.LamTree as LTB
import qualified InstantiatedNaive.LamTree as LTN
import qualified LamTreeB as LTB
import qualified LamTreeN as LTN

-- it's unfortunate that this isn't checked :/
{-# complete AppN, LamN, LitN, VarN #-}
pattern AppN li ri
        = LTN.AppR (LTN.AppFunAST li) (LTN.AppArgAST ri)
pattern LamN m
        = LTN.LamR (LTN.LamAST m)
pattern LitN i
        = LTN.LitR i (())
pattern VarN i
        = LTN.VarR i (())

{-# complete AppB, LamB, LitB, VarB #-}
pattern AppB li ri
        = LTB.AppR (IdentityT li) (IdentityT ri)
pattern LamB m
        = LTB.LamR (LTB.LamAST m)
pattern LitB i
        = LTB.LitR i (())
pattern VarB i
        = LTB.VarR i (())

main :: IO ()
main = putStrLn "Hello, Haskell!"
