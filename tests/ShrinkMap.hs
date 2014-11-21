{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
import Test.QuickCheck
import Data.List

shrinkOrderedList :: (Ord a, Arbitrary a) => [a] -> [[a]]
shrinkOrderedList = shrinkMap sort id

prop_shrinkOrderedList :: [Int] -> Bool
prop_shrinkOrderedList xs = all isSorted (shrinkOrderedList xs)
  where isSorted x = x == sort x 

main = $quickCheckAll
