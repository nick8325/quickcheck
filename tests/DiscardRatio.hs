module Main where

import Control.Monad
import System.Exit
import Test.QuickCheck

assert :: String -> Bool -> IO  ()
assert s False = do
  putStrLn $ s ++ " failed!"
  exitFailure
assert _ _     = pure ()

quickCheckYes, quickCheckNo :: Property -> IO ()
quickCheckYes p = do
  res <- quickCheckResult p
  unless (isSuccess res) exitFailure
quickCheckNo p = do
  res <- quickCheckResult p
  when (isSuccess res) exitFailure

check :: Result -> Int -> Int -> IO ()
check res n d = do
  quickCheckYes $ once $ n === numTests res
  quickCheckYes $ once $ d === numDiscarded res

main :: IO ()
main = do
  putStrLn "Testing: False ==> True"
  res <- quickCheckResult $ withDiscardRatio 2 $ False ==> True
  check res 0 200

  putStrLn "Testing: x == x"
  res <- quickCheckResult $ withDiscardRatio 2 $ \ x -> (x :: Int) == x
  check res 100 0

  -- The real ratio is 20, if 1 works or 40 doesn't it's
  -- probably because we broke something!
  let p50 = forAll (choose (1, 1000)) $ \ x -> (x :: Int) < 50 ==> True
  putStrLn "Expecting failure (discard ratio 1): x < 50 ==> True"
  quickCheckNo $ withDiscardRatio 1 p50
  putStrLn "Expecting success (discard ratio 40): x < 50 ==> True"
  quickCheckYes $ withDiscardRatio 40 p50
