-- Lots of weird examples to test strange corner cases of QuickCheck,
-- especially exception handling and ctrl-C handling.

{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Function

prop = callback (PostTest (\_ _ -> putStrLn "\n\n\napa\n\n\n")) f
  where f :: Int -> Bool
        f _ = undefined

prop2 (Fun _ f :: Fun Int Int) (Fun _ g :: Fun Int Int) x = f (g x) == g (f x)

fibs = 0:1:zipWith (+) fibs (tail fibs)

prop3 n = n >= 0 && n <= 100000 ==> 1000000 `within` (fibs!!(n+50000) + fibs!!(n+50001) == fibs!!(n+50002))
prop4, prop5, prop6 :: Int -> Property
prop4 _ = within 1000000 (loop :: Property)
prop5 _ = within 1000000 (loop :: Test.QuickCheck.Property.Result)
prop6 _ = within 1000000 (loop :: Bool)

revrev (xs :: [Int]) = within 1000 (reverse (reverse xs) == xs)

undef (n :: Int) = undefined :: Bool
undef2 (n :: Int) = undefined :: Property
undef25 (n :: Int) = return undefined :: Property
undef21 (n :: Int) = return (MkProp (MkRose undefined [])) :: Property -- note: this example is bad because we construct a rose tree without protecting the result
undef22 (n :: Int) = undefined :: Test.QuickCheck.Property.Result
undef3 (n :: Int) = undefined :: Property
undef4 (n :: Int) = collect "" (undefined :: Property)
undef5 (n :: Int) = collect (undefined :: String) (undefined :: Property)

data A = A deriving (Eq, Ord, Show)
instance Arbitrary A where
  arbitrary = return A
  shrink = undefined

test :: Int -> A -> Bool
test _ _ = False

loop = loop

prop_loop (n :: Int) (m :: Int) = prop_loop n m :: Bool
main = quickCheck prop_loop

data B = B deriving (Eq, Ord, Show)
instance Arbitrary B where
  arbitrary = return loop
  shrink = loop

prop_loop2 (x :: B) = prop_loop2 x :: Bool

prop_forevershrink =
  forAllShrink arbitrary shrink $ \n -> if n == (0 :: Int) then prop_forevershrink else error "fail"
