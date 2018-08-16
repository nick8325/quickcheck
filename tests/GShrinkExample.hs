{-# LANGUAGE DeriveGeneric, ScopedTypeVariables, TemplateHaskell #-}

module Main where

import GHC.Generics (Generic)
import Test.QuickCheck

data Nat = Z | S Nat deriving (Eq, Show, Generic)


instance Arbitrary Nat

prop_shrink =
  genericShrink (S (S Z)) === [S Z] .&&.
  genericShrink [0::Int] === [[]]

return []

main :: IO ()
main = do True <- $quickCheckAll; return ()
