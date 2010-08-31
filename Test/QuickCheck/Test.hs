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
  ( RandomGen(..)
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
  }
 deriving ( Show, Read )

-- | Result represents the test result
data Result
  = Success                         -- a successful test run
    { labels      :: [(String,Int)] -- ^ labels and frequencies found during all tests
    }
  | GaveUp                          -- given up
    { numTests    :: Int            -- ^ number of successful tests performed
    , labels      :: [(String,Int)] -- ^ labels and frequencies found during all tests
    }
  | Failure                         -- failed test run
    { usedSeed    :: StdGen         -- ^ what seed was used
    , usedSize    :: Int            -- ^ what was the test size
    , reason      :: String         -- ^ what was the reason
    , labels      :: [(String,Int)] -- ^ labels and frequencies found during all successful tests
    }
  | NoExpectedFailure               -- the expected failure did not happen
    { labels      :: [(String,Int)] -- ^ labels and frequencies found during all successful tests
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
quickCheckWithResult args p =
  do tm  <- newTerminal
     rnd <- case replay args of
              Nothing      -> newStdGen
              Just (rnd,_) -> return rnd
     test MkState{ terminal          = tm
                 , maxSuccessTests   = maxSuccess args
                 , maxDiscardedTests = maxDiscard args
                 , computeSize       = case replay args of
                                         Nothing    -> computeSize (maxSuccess args) (maxSize args)
                                         Just (_,s) -> \_ _ -> s
                 , numSuccessTests   = 0
                 , numDiscardedTests = 0
                 , collected         = []
                 , expectedFailure   = False
                 , randomSeed        = rnd
                 , isShrinking       = False
                 , numSuccessShrinks = 0
                 , numTryShrinks     = 0
                 } (unGen (property p))
  where computeSize maxSuccess maxSize n d
          -- e.g. with maxSuccess = 250, maxSize = 100, goes like this:
          -- 0, 1, 2, ..., 99, 0, 1, 2, ..., 99, 0, 2, 4, ..., 98.
          | n `roundTo` maxSize + maxSize <= maxSuccess ||
            n >= maxSuccess ||
            maxSuccess `mod` maxSize == 0 = n `mod` maxSize + d `div` 10
          | otherwise =
            (n `mod` maxSize) * maxSize `div` (maxSuccess `mod` maxSize) + d `div` 10
        n `roundTo` m = (n `div` m) * m

--------------------------------------------------------------------------
-- main test loop

test :: State -> (StdGen -> Int -> Prop) -> IO Result
test st f
  | numSuccessTests st   >= maxSuccessTests st   = doneTesting st f
  | numDiscardedTests st >= maxDiscardedTests st = giveUp st f
  | otherwise                                    = runATest st f

doneTesting :: State -> (StdGen -> Int -> Prop) -> IO Result
doneTesting st f =
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
     if expectedFailure st then
       return Success{ labels = summary st }
      else
       return NoExpectedFailure{ labels = summary st }
  
giveUp :: State -> (StdGen -> Int -> Prop) -> IO Result
giveUp st f =
  do -- CALLBACK gave_up?
     putPart (terminal st)
       ( bold ("*** Gave up!")
      ++ " Passed only "
      ++ show (numSuccessTests st)
      ++ " tests"
       )
     success st
     return GaveUp{ numTests = numSuccessTests st
                  , labels   = summary st
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
     (mres, ts) <- unpackRose (unProp (f rnd1 size))
     res <- mres
     callbackPostTest st res
     
     case ok res of
       Just True -> -- successful test
         do test st{ numSuccessTests = numSuccessTests st + 1
                   , randomSeed      = rnd2
                   , collected       = stamp res : collected st
                   , expectedFailure = expect res
                   } f
       
       Nothing -> -- discarded test
         do test st{ numDiscardedTests = numDiscardedTests st + 1
                   , randomSeed        = rnd2
                   , expectedFailure   = expect res
                   } f
         
       Just False -> -- failed test
         do if expect res
              then putPart (terminal st) (bold "*** Failed! ")
              else putPart (terminal st) "+++ OK, failed as expected. "
            putTemp (terminal st)
              ( short 30 (P.reason res)
             ++ " (after "
             ++ number (numSuccessTests st+1) "test"
             ++ ")..."
              )
            foundFailure st res ts
            if not (expect res) then
              return Success{ labels = summary st }
             else
              return Failure{ usedSeed    = randomSeed st -- correct! (this will be split first)
                            , usedSize    = size
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
  case labels ++ covers of
    []    -> do putLine (terminal st) "."
    [pt]  -> do putLine (terminal st)
                  ( " ("
                 ++ dropWhile isSpace pt
                 ++ ")."
                  )
    cases -> do putLine (terminal st) ":"
                sequence_ [ putLine (terminal st) pt | pt <- cases ]
 where
  labels = reverse
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

foundFailure :: State -> P.Result -> [Rose (IO P.Result)] -> IO ()
foundFailure st res ts =
  do localMin st{ numTryShrinks = 0, isShrinking = True } res ts

localMin :: State -> P.Result -> [Rose (IO P.Result)] -> IO ()
localMin st res _ | P.interrupted res = localMinFound st res
localMin st res ts = do
  r <- tryEvaluate ts
  case r of
    Left err ->
      localMinFound st
         (exception "Exception while generating shrink-list" err)
    Right [] -> localMinFound st res
    Right (t:ts) ->
      do -- CALLBACK before_test
        (mres', ts') <- unpackRose t
        res' <- mres'
        putTemp (terminal st)
          ( short 35 (P.reason res)
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
        callbackPostTest st res'
        if ok res' == Just False
          then foundFailure st{ numSuccessShrinks = numSuccessShrinks st + 1 } res' ts'
          else localMin st{ numTryShrinks = numTryShrinks st + 1 } res ts

localMinFound :: State -> P.Result -> IO ()
localMinFound st res =
  do putLine (terminal st)
       ( P.reason res
      ++ " (after " ++ number (numSuccessTests st+1) "test"
      ++ concat [ " and " ++ number (numSuccessShrinks st) "shrink"
                | numSuccessShrinks st > 0
                ]
      ++ "):  "
       )
     callbackPostFinalFailure st res

--------------------------------------------------------------------------
-- callbacks

callbackPostTest :: State -> P.Result -> IO ()
callbackPostTest st res =
  sequence_ [ f st res | PostTest f <- callbacks res ]

callbackPostFinalFailure :: State -> P.Result -> IO ()
callbackPostFinalFailure st res =
  sequence_ [ f st res | PostFinalFailure f <- callbacks res ]

--------------------------------------------------------------------------
-- the end.
