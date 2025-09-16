import Test.QuickCheck
import Test.QuickCheck.Monadic

import Data.IORef
import System.Exit (exitSuccess, exitFailure)
import Control.Monad (when)

main :: IO ()
main = do
  succ <- newIORef 0
  disc <- newIORef 0

  -- test 1  
  quickCheck (propTestWithProgress succ)
  i <- readIORef succ
  when (i /= 99) exitFailure

  modifyIORef succ (const 0)

  -- test 2
  quickCheck (proptestWithProgressDiscard succ disc)
  i1 <- readIORef succ
  i2 <- readIORef disc
  putStrLn $ show i1
  putStrLn $ show i2
  when (i1 /= 0 || i2 /= 999) exitFailure

  modifyIORef succ (const 0)
  modifyIORef disc (const 0)

  -- test 3
  quickCheck (proptestWithProgressNotInstalled succ disc)
  i1 <- readIORef succ
  i2 <- readIORef disc
  putStrLn $ show i1
  putStrLn $ show i2
  when (i1 /= 0 || i2 /= 0) exitFailure

  exitSuccess

-- all these tests succeed, incrementing the counter every time
propTestWithProgress :: IORef Int -> Property
propTestWithProgress ref =
    forAll (arbitrary :: Gen Int) $ \n ->
        withProgress (\p -> modifyIORef ref (\_ -> currentPassed p))
        $ n == n

-- all of these tests are discarded, never updating the currentPassed counter, but always
-- the currentDiscarded one.
proptestWithProgressDiscard :: IORef Int -> IORef Int -> Property
proptestWithProgressDiscard succ disc =
    forAll (arbitrary :: Gen Int) $ \n ->
        withProgress (\p -> do
          modifyIORef succ (\_ -> currentPassed p)
          modifyIORef disc (\_ -> currentDiscarded p))
        $ n > 1000 ==>
          n == n

-- The callback is installed after the test on n, meaning that since all tests are
-- discarded, it will never be called. It will never be executed.
proptestWithProgressNotInstalled :: IORef Int -> IORef Int -> Property
proptestWithProgressNotInstalled succ disc =
    forAll (arbitrary :: Gen Int) $ \n ->
      n > 1000 ==>
        withProgress (\p -> do
          modifyIORef succ (\_ -> currentPassed p)
          modifyIORef disc (\_ -> currentDiscarded p))
        $ n == n