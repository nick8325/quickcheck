{-# LANGUAGE TemplateHaskell, RecursiveDo #-}
import Test.QuickCheck
import Control.Monad.Fix

-- A simple (not complete) test for the MonadFix instance.
cyclicList :: Gen [Int]
cyclicList = do
  rec xs <- fmap (:ys) arbitrary
      ys <- fmap (:xs) arbitrary
  return xs

prop_cyclic :: Property
prop_cyclic =
  forAll (Blind <$> cyclicList) $ \(Blind xs) ->
    -- repeats with period 2
    and $ take 100 $ zipWith (==) xs (drop 2 xs)

prop_period2 :: Property
prop_period2 =
  expectFailure $
  forAll (Blind <$> cyclicList) $ \(Blind xs) ->
    -- does not always repeat with period 1
    and $ take 100 $ zipWith (==) xs (drop 1 xs)

return []
main = do True <- $quickCheckAll; return ()
