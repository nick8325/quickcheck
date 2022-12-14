{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Data.List
import           System.Exit                    ( exitFailure )

import           Test.QuickCheck.Monadic
import           Test.QuickCheck
import           Test.QuickCheck.Random
import Data.IORef
import System.Directory


prop_test_numgen :: IORef Int -> Property
prop_test_numgen vi = monadicIO $ do

    i <- run $ atomicModifyIORef' vi $ \i -> (i + 1, i)

    run $ writeFile (show i ++ "file.txt") "hi"

    assert True

-- the ioref = unique number gen shared by threads, in this case. Just for illustration
prop_test_cleanup :: IORef Int -> Property
prop_test_cleanup vi = monadicIO $ do
    i <- run $ atomicModifyIORef' vi $ \i -> (i + 1, i)

    c <- graceful
           (do writeFile (show i ++ "file.txt") "hi"
               c <- readFile (show i ++ "file.txt")
               removeFile (show i ++ "file.txt")
               return c)

           (do b <- doesFileExist (show i ++ "file.txt")
               if b then removeFile (show i ++ "file.txt") else return ())

    assert (c == "hi")

prop_graceful :: Property
prop_graceful = monadicIO $ do
    -- first IO action that might leave unwanted artifacts
    c1 <- graceful
            (do writeFile "firstfile.txt" "hi"
                c <- readFile "firstfile.txt"
                removeFile "firstfile.txt"
                return c)
            -- cleanup function only called if ctrl-c pressed
            (do b <- doesFileExist "firstfile.txt"
                if b then removeFile "firstfile.txt" else return ())

    -- second IO action that might leave unwanted artifacts
    c2 <- graceful
           (do writeFile "secondfile.txt" "hi"
               c <- readFile "secondfile.txt"
               removeFile "secondfile.txt"
               return c)
           -- cleanup function only called if ctrl-c pressed
           (do b <- doesFileExist "secondfile.txt"
               if b then removeFile "secondfile.txt" else return ())

    -- donkey property
    assert (c1 == c2)

-- import           Criterion
-- import           Criterion.Main
-- import           Criterion.Types
-- import           Statistics.Types

--prop_test :: [Int] -> Property
prop_test :: [Int] -> Bool
prop_test xs = length xs >= length (nub (reverse xs))
--prop_test xs = not $ length xs == 23
--prop_test xs = length xs == 20 ==> True
--prop_test xs = length xs > length (reverse xs) ==> True
--prop_test xs = cover 20 (length xs > 1) "longer than one" $ length xs >= length (nub (reverse xs))

--main :: IO ()
--main = quickCheckPar prop_test
--  if isSuccess r
--      then return ()
--      else exitFailure

-- getTime :: Report -> Double
-- getTime r = estPoint $ anMean $ reportAnalysis r

numSuccess :: Int
numSuccess = 1000000

myArgs :: Args
myArgs = stdArgs { maxSuccess = numSuccess, chatty = True{-, replay = Just (read "SMGen 6194963116934691259 7575707605799764569" :: QCGen, 58) -}}

-- withNTesters :: Int -> IO ()
-- withNTesters cores = do
--     reports <- sequence $ map
--         (\n -> benchmark' $ nfIO $ quickCheckParWith'
--             myArgs
--             (stdParArgs { numTesters = n })
--             prop_test
--         )
--         [1 .. cores]
--     appendFile "report.txt" $ show (map getTime reports) ++ "\n"

main2 :: IO ()
main2 = do
    vi <- newIORef 0
    quickCheckParWith myArgs (stdParArgs { numTesters = 4 }) $ prop_test_cleanup vi

prop_reverse :: [Int] -> Bool
prop_reverse xs = reverse (reverse xs) == xs

main :: IO ()
main = do
    quickCheckParWith stdArgs (stdParArgs { numTesters = 4, rightToWorkSteal = False }) $ withMaxSuccess 10000 prop_reverse
--  main2
--    quickCheckPar $ withMaxSuccess 1000000 prop_test
--    quickCheckWith myArgs prop_test
--    quickCheckParWith' myArgs (stdParArgs { numTesters = 4, sizeStrategy = Stride }) $ prop_test--withMaxSuccess 1000 prop_test
    -- withNTesters 1
    -- withNTesters 24
    -- withNTesters 24
    -- withNTesters 24
    -- withNTesters 24
    -- withNTesters 24
    -- withNTesters 24
    -- withNTesters 24
    -- withNTesters 24
    -- withNTesters 24

{-
[ 16.047
, 8.576
, 5.969
, 4.617
, 3.846
, 3.297
, 2.928
, 2.667
, 2.386
, 2.215
, 2.077
, 2.001
, 2.254
, 2.365
, 2.327
, 2.297
, 2.266
, 2.247
, 2.096
, 2.097
, 2.006
, 1.957
, 1.816
, 1.784
]


-}
