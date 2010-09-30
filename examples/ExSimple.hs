{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
module Main where

--------------------------------------------------------------------------
-- imports

--import Debug.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.TH

--------------------------------------------------------------------------
-- example 1

allEqual  x y z = x == y && y == z
allEqual' x y z = 2*x == y + z

prop_SimonThompson x y (z :: Int) =
  allEqual x y z == allEqual' x y z

--------------------------------------------------------------------------
-- example 2

prop_ReverseReverse xs =
  reverse (reverse xs) == xs

--------------------------------------------------------------------------
-- example 3

prop_Error (x,y) =
  2*x <= 5*y
    
--------------------------------------------------------------------------
-- main

main =
  do quickCheck prop_SimonThompson
     quickCheck $(mono 'prop_ReverseReverse)

addQuickCheckAll

--------------------------------------------------------------------------
-- the end.
