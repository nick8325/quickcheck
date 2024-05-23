module Main where

import Control.Monad
import System.Exit
import Test.QuickCheck

assert :: String -> Bool -> IO  ()
assert s False = do
  putStrLn $ s ++ " failed!"
  exitFailure
assert _ _     = return ()

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

  -- Annoying interactions of discard and cover
  quickCheckYes $ forAllBlind (oneof [pure True, pure discard]) $ \ b -> cover 10 b "b" True
  quickCheck $ cover 10 discard "b" True
  quickCheck $ classify "b" discard True
