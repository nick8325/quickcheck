{-# LANGUAGE DeriveGeneric, ScopedTypeVariables, TemplateHaskell #-}

module Main where

import GHC.Generics (Generic)
import Test.QuickCheck
import Test.QuickCheck.Function

data D a = C1 a | C2 deriving (Eq, Show, Read, Generic)


instance Arbitrary a => Arbitrary (D a) where arbitrary = error "not implemented"
instance CoArbitrary a => CoArbitrary (D a)

instance (Show a, Read a) => Function (D a) where
  function = functionShow

prop_coarbitrary (Fun _ f) =
  expectFailure $
  withNumTests 1000 $
  f (C1 (2::Int)) `elem` [0, 1 :: Int]

return []
main = do True <- $quickCheckAll; return ()
