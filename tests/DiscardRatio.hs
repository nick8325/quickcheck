module Main where

import Control.Monad
import System.Exit
import Test.QuickCheck

assert :: String -> Bool -> IO  ()
assert s False = do
  putStrLn $ s ++ " failed!"
  exitFailure
assert _ _     = pure ()

quickCheckYesWith, quickCheckNoWith :: Testable p => Args -> p -> IO ()
quickCheckYesWith args p = do
  res <- quickCheckWithResult args p
  unless (isSuccess res) exitFailure
quickCheckNoWith args p = do
  res <- quickCheckWithResult args p
  when (isSuccess res) exitFailure
quickCheckYes, quickCheckNo :: Testable p => p -> IO ()
quickCheckYes = quickCheckYesWith stdArgs
quickCheckNo = quickCheckNoWith stdArgs

check :: Result -> Int -> Int -> IO ()
check res n d = do
  quickCheckYes $ once $ n === numTests res
  quickCheckYes $ once $ d === numDiscarded res

main :: IO ()
main = do
  putStrLn "Expecting gave up after 200 tries: False ==> True"
  res <- quickCheckResult $ withDiscardRatio 2 $ False ==> True
  check res 0 200
  res <- quickCheckWithResult stdArgs{maxDiscardRatio = 2} $ False ==> True
  check res 0 200

  putStrLn "\nExpecting success after 100 tests: x == x"
  res <- quickCheckResult $ withDiscardRatio 2 $ \ x -> (x :: Int) == x
  check res 100 0
  res <- quickCheckWithResult stdArgs{maxDiscardRatio = 2} $ \ x -> (x :: Int) == x
  check res 100 0

  -- The real ratio is 20, if 1 works or 40 doesn't it's
  -- probably because we broke something!
  let p50 = forAll (choose (1, 1000)) $ \ x -> (x :: Int) < 50 ==> True
  putStrLn "\nExpecting failure (discard ratio 1): x < 50 ==> True"
  quickCheckNo $ withDiscardRatio 1 p50
  quickCheckNoWith stdArgs{maxDiscardRatio = 1} p50
  putStrLn "\nExpecting success (discard ratio 40): x < 50 ==> True"
  quickCheckYes $ withDiscardRatio 40 p50
  quickCheckYesWith stdArgs{maxDiscardRatio = 40} p50

  -- This was brought to our attention by @robx in issue #338
  let p k k' = k /= k' ==> (k :: Int) /= k'
  putStrLn "\nExpecting success (maxSuccess = 1): k /= k' ==> k /= k'"
  quickCheckYes $ withMaxSuccess 1 p
  quickCheckYesWith stdArgs{maxSuccess = 1} p
