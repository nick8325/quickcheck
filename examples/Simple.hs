{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
module Main where

--------------------------------------------------------------------------
-- imports

import Test.QuickCheck
import Test.QuickCheck.All

--------------------------------------------------------------------------
-- example 1

allEqual  x y z = x == y && y == z
allEqual' x y z = 2*x == y + z

prop_SimonThompson x y (z :: Int) =
  allEqual x y z == allEqual' x y z

--------------------------------------------------------------------------
-- example 2

prop_ReverseReverse :: Eq a => [a] -> Bool
prop_ReverseReverse xs =
  reverse (reverse xs) == xs

prop_Reverse xs =
  reverse xs == xs

{-
prop_Blahblah
-}

--------------------------------------------------------------------------
-- example 3

prop_Error (x,y) =
  2*x <= 5*y
    
--------------------------------------------------------------------------
-- main

main = $(quickCheckAll)
conj = whenFailPrint "Simon Thompson" $(mono 'prop_SimonThompson) .&&.
       whenFailPrint "reverse" $(mono 'prop_Reverse)
disj = whenFailPrint "Simon Thompson" $(mono 'prop_SimonThompson) .||.
       whenFailPrint "reverse" $(mono 'prop_Reverse)

--------------------------------------------------------------------------
-- the end.
