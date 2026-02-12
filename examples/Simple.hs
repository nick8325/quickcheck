{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
module Main where

--------------------------------------------------------------------------
-- imports

import Test.QuickCheck

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

--------------------------------------------------------------------------
-- example 3

prop_Error (x,y) =
  2*x <= 5*y

--------------------------------------------------------------------------
-- main

return []
prop_conj = counterexample "Simon Thompson" $(monomorphic 'prop_SimonThompson) .&&.
            counterexample "reverse" $(monomorphic 'prop_Reverse)
prop_disj = counterexample "reverse" $(monomorphic 'prop_Reverse) .||.
            counterexample "Simon Thompson" $(monomorphic 'prop_SimonThompson)
return []
main = do True <- $quickCheckAll; return ()

--------------------------------------------------------------------------
-- the end.
