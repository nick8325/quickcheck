{-# LANGUAGE DeriveGeneric, ScopedTypeVariables #-}

module GShrinkExample where

import GHC.Generics (Generic)
import Test.QuickCheck

data Nat = Z | S Nat deriving (Eq, Show, Generic)


instance Arbitrary Nat


main :: IO ()
main = do
  print $ genericShrink (S (S Z)) == [S Z]
  print $ genericShrink [0::Int] == [[]]
