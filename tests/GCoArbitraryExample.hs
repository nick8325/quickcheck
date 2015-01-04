{-# LANGUAGE DeriveGeneric, ScopedTypeVariables #-}

module GCoArbitraryExample where

import GHC.Generics (Generic)
import Test.QuickCheck
import Test.QuickCheck.Function

data D a = C1 a | C2 deriving (Eq, Show, Read, Generic)


instance Arbitrary a => Arbitrary (D a)
instance CoArbitrary a => CoArbitrary (D a)

instance (Show a, Read a) => Function (D a) where
  function = functionShow

main :: IO ()
main = quickCheck $ \(Fun _ f) ->
  f (C1 (2::Int)) `elem` [0, 1 :: Int]
