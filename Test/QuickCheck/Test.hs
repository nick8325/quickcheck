module Test.QuickCheck.Test where

--------------------------------------------------------------------------
-- imports

import Test.QuickCheck.Gen
import Test.QuickCheck.Property hiding ( Result( reason, interrupted ) )
import qualified Test.QuickCheck.Property as P
import Test.QuickCheck.Text
import Test.QuickCheck.State
import Test.QuickCheck.Exception

import System.Random
  ( split
  , newStdGen
  , StdGen
  )

import Data.Char
  ( isSpace
  )

import Data.List
  ( sort
  , group
  , groupBy
  , intersperse
  )
--------------------------------------------------------------------------
-- quickCheck

-- * Running tests

-- | Args specifies arguments to the QuickCheck driver
data Args
  = Args
  { replay     :: Maybe (StdGen,Int) -- ^ should we replay a previous test?
  , maxSuccess :: Int                -- ^ maximum number of successful tests before succeeding
  , maxDiscard :: Int                -- ^ maximum number of discarded tests before giving up
  , maxSize    :: Int                -- ^ size to use for the biggest test cases
  , chatty     :: Bool               -- ^ whether to print anything
  }
 deriving ( Show, Read )

-- | Result represents the test result
data Result
  = Success                            -- a successful test run
    { numTests       :: Int            -- ^ number of successful tests performed
    , labels         :: [(String,Int)] -- ^ labels and frequencies found during all tests
    , output         :: String         -- ^ printed output
    }
  | GaveUp                             -- given up
    { numTests       :: Int            -- ^ number of successful tests performed
    , labels         :: [(String,Int)] -- ^ labels and frequencies found during all tests
    , output         :: String         -- ^ printed output
    }
  | Failure                            -- failed test run
    { numTests       :: Int            -- ^ number of tests performed
    , numShrinks     :: Int            -- ^ number of successful shrinking steps performed
    , usedSeed       :: StdGen         -- ^ what seed was used
    , usedSize       :: Int            -- ^ what was the test size
    , reason         :: String         -- ^ what was the reason
    , labels         :: [(String,Int)] -- ^ labels and frequencies found during all successful tests
    , output         :: String         -- ^ printed output
    }
  | NoExpectedFailure                  -- the expected failure did not happen
    { numTests       :: Int            -- ^ number of tests performed
    , labels         :: [(String,Int)] -- ^ labels and frequencies found during all successful tests
    , output         :: String         -- ^ printed output
    }
 deriving ( Show, Read )

-- | isSuccess checks if the test run result was a success
isSuccess :: Result -> Bool
isSuccess Success{} = True
isSuccess _         = False

-- | stdArgs are the default test arguments used
stdArgs :: Args
stdArgs = Args
  { replay     = Nothing
  , maxSuccess = 100
  , maxDiscard = 500
  , maxSize    = 100
  , chatty     = True
-- noShrinking flag?
  }

-- | Tests a property and prints the results to 'stdout'.
quickCheck :: Testable prop => prop -> IO ()
quickCheck p = quickCheckWith stdArgs p

-- | Tests a property, using test arguments, and prints the results to 'stdout'.
quickCheckWith :: Testable prop => Args -> prop -> IO ()
quickCheckWith args p = quickCheckWithResult args p >> return ()

-- | Tests a property, produces a test result, and prints the results to 'stdout'.
quickCheckResult :: Testable prop => prop -> IO Result
quickCheckResult p = quickCheckWithResult stdArgs p

-- | Tests a property, using test arguments, produces a test result, and prints the results to 'stdout'.
quickCheckWithResult :: Testable prop => Args -> prop -> IO Result
quickCheckWithResult a p =
  do tm <- if chatty a then newStdioTerminal else newNullTerminal
     rnd <- case replay a of
              Nothing      -> newStdGen
              Just (rnd,_) -> return rnd
     test MkState{ terminal          = tm
                 , maxSuccessTests   = if exhaustive p then 1 else maxSuccess a
                 , maxDiscardedTests = maxDiscard a
                 , computeSize       = case replay a of
                                         Nothing    -> computeSize'
                                         Just (_,s) -> computeSize' `at0` s
                 , numSuccessTests   = 0
                 , numDiscardedTests = 0
                 , collected         = []
                 , expectedFailure   = False
                 , randomSeed        = rnd
                 , numSuccessShrinks = 0
                 , numTryShrinks     = 0
                 , numTotTryShrinks  = 0
                 } (unGen (property p))
  where computeSize' n d
          -- e.g. with maxSuccess = 250, maxSize = 100, goes like this:
          -- 0, 1, 2, ..., 99, 0, 1, 2, ..., 99, 0, 2, 4, ..., 98.
          | n `roundTo` maxSize a + maxSize a <= maxSuccess a ||
            n >= maxSuccess a ||
            maxSuccess a `mod` maxSize a == 0 = n `mod` maxSize a + d `div` 10
          | otherwise =
            (n `mod` maxSize a) * maxSize a `div` (maxSuccess a `mod` maxSize a) + d `div` 10
        n `roundTo` m = (n `div` m) * m
        at0 f s 0 0 = s
        at0 f s n d = f n d

-- | Tests a property and prints the results and all test cases generated to 'stdout'.
-- This is just a convenience function that means the same as 'quickCheck' '.' 'verbose'.
verboseCheck :: Testable prop => prop -> IO ()
verboseCheck p = quickCheck (verbose p)

-- | Tests a property, using test arguments, and prints the results and all test cases generated to 'stdout'.
-- This is just a convenience function that combines 'quickCheckWith' and 'verbose'.
verboseCheckWith :: Testable prop => Args -> prop -> IO ()
verboseCheckWith args p = quickCheckWith args (verbose p)

-- | Tests a property, produces a test result, and prints the results and all test cases generated to 'stdout'.
-- This is just a convenience function that combines 'quickCheckResult' and 'verbose'.
verboseCheckResult :: Testable prop => prop -> IO Result
verboseCheckResult p = quickCheckResult (verbose p)

-- | Tests a property, using test arguments, produces a test result, and prints the results and all test cases generated to 'stdout'.
-- This is just a convenience function that combines 'quickCheckWithResult' and 'verbose'.
verboseCheckWithResult :: Testable prop => Args -> prop -> IO Result
verboseCheckWithResult a p = quickCheckWithResult a (verbose p)

--------------------------------------------------------------------------
-- main test loop

test :: State -> (StdGen -> Int -> Prop) -> IO Result
test st f
  | numSuccessTests st   >= maxSuccessTests st   = doneTesting st f
  | numDiscardedTests st >= maxDiscardedTests st = giveUp st f
  | otherwise                                    = runATest st f

doneTesting :: State -> (StdGen -> Int -> Prop) -> IO Result
doneTesting st _f =
  do -- CALLBACK done_testing?
     if expectedFailure st then
       putPart (terminal st)
         ( "+++ OK, passed "
        ++ show (numSuccessTests st)
        ++ " tests"
         )
      else
       putPart (terminal st)
         ( bold ("*** Failed!")
        ++ " Passed "
        ++ show (numSuccessTests st)
        ++ " tests (expected failure)"
         )
     success st
     theOutput <- terminalOutput (terminal st)
     if expectedFailure st then
       return Success{ labels = summary st,
                       numTests = numSuccessTests st,
                       output = theOutput }
      else
       return NoExpectedFailure{ labels = summary st,
                                 numTests = numSuccessTests st,
                                 output = theOutput }
  
giveUp :: State -> (StdGen -> Int -> Prop) -> IO Result
giveUp st _f =
  do -- CALLBACK gave_up?
     putPart (terminal st)
       ( bold ("*** Gave up!")
      ++ " Passed only "
      ++ show (numSuccessTests st)
      ++ " tests"
       )
     success st
     theOutput <- terminalOutput (terminal st)
     return GaveUp{ numTests = numSuccessTests st
                  , labels   = summary st
                  , output   = theOutput
                  }

runATest :: State -> (StdGen -> Int -> Prop) -> IO Result
runATest st f =
  do -- CALLBACK before_test
     putTemp (terminal st)
        ( "("
       ++ number (numSuccessTests st) "test"
       ++ concat [ "; " ++ show (numDiscardedTests st) ++ " discarded"
                 | numDiscardedTests st > 0
                 ]
       ++ ")"
        )
     let size = computeSize st (numSuccessTests st) (numDiscardedTests st)
     MkRose res ts <- protectRose (reduceRose (unProp (f rnd1 size)))
     callbackPostTest st res
  
     let continue break st' | abort res = break st'
                            | otherwise = test st'
     
     case res of
       MkResult{ok = Just True, stamp = stamp, expect = expect} -> -- successful test
         do continue doneTesting
              st{ numSuccessTests = numSuccessTests st + 1
                , randomSeed      = rnd2
                , collected       = stamp : collected st
                , expectedFailure = expect
                } f
       
       MkResult{ok = Nothing, expect = expect} -> -- discarded test
         do continue giveUp 
              st{ numDiscardedTests = numDiscardedTests st + 1
                , randomSeed        = rnd2
                , expectedFailure   = expect
                } f
         
       MkResult{ok = Just False} -> -- failed test
         do if expect res
              then putPart (terminal st) (bold "*** Failed! ")
              else putPart (terminal st) "+++ OK, failed as expected. "
            numShrinks <- foundFailure st res ts
            theOutput <- terminalOutput (terminal st)
            if not (expect res) then
              return Success{ labels = summary st,
                              numTests = numSuccessTests st+1,
                              output = theOutput }
             else
              return Failure{ usedSeed    = randomSeed st -- correct! (this will be split first)
                            , usedSize    = size
                            , numTests    = numSuccessTests st+1
                            , numShrinks  = numShrinks
                            , output      = theOutput
                            , reason      = P.reason res
                            , labels      = summary st
                            }
 where
  (rnd1,rnd2) = split (randomSeed st)

summary :: State -> [(String,Int)]
summary st = reverse
           . sort
           . map (\ss -> (head ss, (length ss * 100) `div` numSuccessTests st))
           . group
           . sort
           $ [ concat (intersperse ", " s')
             | s <- collected st
             , let s' = [ t | (t,_) <- s ]
             , not (null s')
             ]

success :: State -> IO ()
success st =
  case allLabels ++ covers of
    []    -> do putLine (terminal st) "."
    [pt]  -> do putLine (terminal st)
                  ( " ("
                 ++ dropWhile isSpace pt
                 ++ ")."
                  )
    cases -> do putLine (terminal st) ":"
                sequence_ [ putLine (terminal st) pt | pt <- cases ]
 where
  allLabels = reverse
            . sort
            . map (\ss -> (showP ((length ss * 100) `div` numSuccessTests st) ++ head ss))
            . group
            . sort
            $ [ concat (intersperse ", " s')
              | s <- collected st
              , let s' = [ t | (t,0) <- s ]
              , not (null s')
              ]
  
  covers = [ ("only " ++ show occurP ++ "% " ++ fst (head lps) ++ "; not " ++ show reqP ++ "%")
           | lps <- groupBy first
                  . sort
                  $ [ lp
                    | lps <- collected st
                    , lp <- maxi lps
                    , snd lp > 0
                    ]
           , let occurP = (100 * length lps) `div` maxSuccessTests st
                 reqP   = maximum (map snd lps)
           , occurP < reqP
           ]
  
  (x,_) `first` (y,_) = x == y 

  maxi = map (\lps -> (fst (head lps), maximum (map snd lps)))
       . groupBy first
       . sort

  showP p = (if p < 10 then " " else "") ++ show p ++ "% "

--------------------------------------------------------------------------
-- main shrinking loop

foundFailure :: State -> P.Result -> [Rose P.Result] -> IO Int
foundFailure st res ts =
  do localMin st{ numTryShrinks = 0 } res ts

localMin :: State -> P.Result -> [Rose P.Result] -> IO Int
localMin st res _ | P.interrupted res = localMinFound st res
localMin st res ts = do
  putTemp (terminal st)
    ( short 26 (oneLine (P.reason res))
   ++ " (after " ++ number (numSuccessTests st+1) "test"
   ++ concat [ " and "
            ++ show (numSuccessShrinks st)
            ++ concat [ "." ++ show (numTryShrinks st) | numTryShrinks st > 0 ]
            ++ " shrink"
            ++ (if numSuccessShrinks st == 1
                && numTryShrinks st == 0
                then "" else "s")
             | numSuccessShrinks st > 0 || numTryShrinks st > 0
             ]
   ++ ")..."
    )
  r <- tryEvaluate ts
  case r of
    Left err ->
      localMinFound st
         (exception "Exception while generating shrink-list" err) { callbacks = callbacks res }
    Right ts' -> localMin' st res ts'

localMin' :: State -> P.Result -> [Rose P.Result] -> IO Int
localMin' st res [] = localMinFound st res
localMin' st res (t:ts) =
  do -- CALLBACK before_test
    MkRose res' ts' <- protectRose (reduceRose t)
    callbackPostTest st res'
    if ok res' == Just False
      then foundFailure st{ numSuccessShrinks = numSuccessShrinks st + 1 } res' ts'
      else localMin st{ numTryShrinks    = numTryShrinks st + 1,
                        numTotTryShrinks = numTotTryShrinks st + 1 } res ts

localMinFound :: State -> P.Result -> IO Int
localMinFound st res =
  do let report = concat [
           "(after " ++ number (numSuccessTests st+1) "test",
           concat [ " and " ++ number (numSuccessShrinks st) "shrink"
                  | numSuccessShrinks st > 0
                  ],
           "): "
           ]
     if isOneLine (P.reason res)
       then putLine (terminal st) (P.reason res ++ " " ++ report)
       else do
         putLine (terminal st) report
         sequence_
           [ putLine (terminal st) msg
           | msg <- lines (P.reason res)
           ]
     callbackPostFinalFailure st res
     return (numSuccessShrinks st)

--------------------------------------------------------------------------
-- callbacks

callbackPostTest :: State -> P.Result -> IO ()
callbackPostTest st res =
  sequence_ [ safely st (f st res) | PostTest _ f <- callbacks res ]

callbackPostFinalFailure :: State -> P.Result -> IO ()
callbackPostFinalFailure st res =
  sequence_ [ safely st (f st res) | PostFinalFailure _ f <- callbacks res ]

safely :: State -> IO () -> IO ()
safely st x = do
  r <- tryEvaluateIO x
  case r of
    Left e ->
      putLine (terminal st)
        ("*** Exception in callback: " ++ show e)
    Right x ->
      return x

--------------------------------------------------------------------------
-- the end.
