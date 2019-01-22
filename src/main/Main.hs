{-# language PatternSynonyms #-}

module Main where

import Data.Functor.Const
import Data.Functor.Identity
import qualified InstantiatedDeBruijn.LamTree as LTB
import qualified InstantiatedNaive.LamTree as LTN
import qualified LamTreeB as LTB
import qualified LamTreeN as LTN

-- it's unfortunate that this isn't checked :/
{-# complete AppN, LamN, LitN #-}
pattern AppN li ri
        = LTN.AppR (LTN.AppFunAST ri) (LTN.AppArgAST li)
pattern LamN m
        = LTN.LamR (LTN.LamAST m)
pattern LitN i
        = LTN.LitR i (LTN.LitAnn ())

{-# complete AppB, LamB, LitB #-}
pattern AppB li ri
        = LTB.AppR (LTB.AppFunAST ri) (LTB.AppArgAST li)
pattern LamB m
        = LTB.LamR (LTB.LamAST m)
pattern LitB i
        = LTB.LitR i (LTB.LitAnn ())

main :: IO ()
main = putStrLn "Hello, Haskell!"
