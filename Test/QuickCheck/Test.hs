-- | The main test loop.
{-# LANGUAGE CPP #-}
#ifndef NO_SAFE_HASKELL
{-# LANGUAGE Safe #-}
#endif
module Test.QuickCheck.Test where

--------------------------------------------------------------------------
-- imports

import Test.QuickCheck.Gen
import Test.QuickCheck.Property hiding ( Result( reason, theException, labels, classifications, coverage ) )
import qualified Test.QuickCheck.Property as P
import Test.QuickCheck.Text
import Test.QuickCheck.State hiding (labels, classifications, coverage)
import qualified Test.QuickCheck.State as S
import Test.QuickCheck.Exception
import Test.QuickCheck.Random
import System.Random(split)
#if defined(MIN_VERSION_containers)
#if MIN_VERSION_containers(0,5,0)
import qualified Data.Map.Strict as Map
#else
import qualified Data.Map as Map
#endif
#else
import qualified Data.Map as Map
#endif
import qualified Data.Set as Set
import Data.Set(Set)
import Data.Map(Map)

import Data.Char
  ( isSpace
  )

import Data.List
  ( sort
  , sortBy
  , group
  , intersperse
  , intercalate
  )

import Data.Maybe(fromMaybe)
import Data.Ord(comparing)
import Text.Printf(printf)
import Data.Either(lefts, rights)

--------------------------------------------------------------------------
-- quickCheck

-- * Running tests

-- | Args specifies arguments to the QuickCheck driver
data Args
  = Args
  { replay          :: Maybe (QCGen,Int)
    -- ^ Should we replay a previous test?
    -- Note: saving a seed from one version of QuickCheck and
    -- replaying it in another is not supported.
    -- If you want to store a test case permanently you should save
    -- the test case itself.
  , maxSuccess      :: Int
    -- ^ Maximum number of successful tests before succeeding. Testing stops
    -- at the first failure. If all tests are passing and you want to run more tests,
    -- increase this number.
  , maxDiscardRatio :: Int
    -- ^ Maximum number of discarded tests per successful test before giving up
  , maxSize         :: Int
    -- ^ Size to use for the biggest test cases
  , chatty          :: Bool
    -- ^ Whether to print anything
  , maxShrinks      :: Int
    -- ^ Maximum number of shrinks to before giving up. Setting this to zero
    --   turns shrinking off.
  }
 deriving ( Show, Read )

-- | Result represents the test result
data Result
  -- | A successful test run
  = Success
    { numTests       :: Int               -- ^ Number of tests performed
    , labels          :: !(Map [String] Int)
    , classifications :: !(Map String (Map String Int))
    , coverage        :: !(Map String (Map String Double))
    , output         :: String            -- ^ Printed output
    }
  -- | Given up
  | GaveUp
    { numTests       :: Int               --   Number of tests performed
    , labels                    :: !(Map [String] Int)
    , classifications           :: !(Map String (Map String Int))
    , coverage                  :: !(Map String (Map String Double))
    , output         :: String            --   Printed output
    }
  -- | A failed test run
  | Failure
    { numTests        :: Int               --   Number of tests performed
    , numShrinks      :: Int               -- ^ Number of successful shrinking steps performed
    , numShrinkTries  :: Int               -- ^ Number of unsuccessful shrinking steps performed
    , numShrinkFinal  :: Int               -- ^ Number of unsuccessful shrinking steps performed since last successful shrink
    , usedSeed        :: QCGen             -- ^ What seed was used
    , usedSize        :: Int               -- ^ What was the test size
    , reason          :: String            -- ^ Why did the property fail
    , theException    :: Maybe AnException -- ^ The exception the property threw, if any
    , output          :: String            --   Printed output
    , failingTestCase :: [String]          -- ^ The test case which provoked the failure
    , features        :: Set String
    }
  -- | A property that should have failed did not
  | NoExpectedFailure
    { numTests       :: Int               --   Number of tests performed
    , labels                    :: !(Map [String] Int)
    , classifications           :: !(Map String (Map String Int))
    , coverage                  :: !(Map String (Map String Double))
    , output         :: String            --   Printed output
    }
 -- | The tests passed but a use of 'cover' had insufficient coverage
 | InsufficientCoverage
    { numTests       :: Int               --   Number of tests performed
    , labels                    :: !(Map [String] Int)
    , classifications           :: !(Map String (Map String Int))
    , coverage                  :: !(Map String (Map String Double))
    , output         :: String            --   Printed output
    }
 deriving ( Show )

-- | Check if the test run result was a success
isSuccess :: Result -> Bool
isSuccess Success{} = True
isSuccess _         = False

-- | The default test arguments
stdArgs :: Args
stdArgs = Args
  { replay          = Nothing
  , maxSuccess      = 100
  , maxDiscardRatio = 10
  , maxSize         = 100
  , chatty          = True
  , maxShrinks      = maxBound
  }

-- | Tests a property and prints the results to 'stdout'.
--
-- By default up to 100 tests are performed, which may not be enough
-- to find all bugs. To run more tests, use 'withMaxSuccess'.
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
  withState a (\s -> test s (unGen (unProperty (property p))))

withState :: Args -> (State -> IO a) -> IO a
withState a test = (if chatty a then withStdioTerminal else withNullTerminal) $ \tm -> do
     rnd <- case replay a of
              Nothing      -> newQCGen
              Just (rnd,_) -> return rnd
     test MkState{ terminal                  = tm
                 , maxSuccessTests           = maxSuccess a
                 , maxDiscardedRatio         = maxDiscardRatio a
                 , computeSize               = case replay a of
                                                 Nothing    -> computeSize'
                                                 Just (_,s) -> computeSize' `at0` s
                 , numTotMaxShrinks          = maxShrinks a
                 , numSuccessTests           = 0
                 , numDiscardedTests         = 0
                 , numRecentlyDiscardedTests = 0
                 , S.labels                  = Map.empty
                 , S.classifications         = Map.empty
                 , S.coverage                = Map.empty
                 , expected                  = True
                 , randomSeed                = rnd
                 , numSuccessShrinks         = 0
                 , numTryShrinks             = 0
                 , numTotTryShrinks          = 0
                 }
  where computeSize' n d
          -- e.g. with maxSuccess = 250, maxSize = 100, goes like this:
          -- 0, 1, 2, ..., 99, 0, 1, 2, ..., 99, 0, 2, 4, ..., 98.
          | n `roundTo` maxSize a + maxSize a <= maxSuccess a ||
            n >= maxSuccess a ||
            maxSuccess a `mod` maxSize a == 0 = (n `mod` maxSize a + d `div` 10) `min` maxSize a
          | otherwise =
            ((n `mod` maxSize a) * maxSize a `div` (maxSuccess a `mod` maxSize a) + d `div` 10) `min` maxSize a
        n `roundTo` m = (n `div` m) * m
        at0 f s 0 0 = s
        at0 f s n d = f n d

-- | Tests a property and prints the results and all test cases generated to 'stdout'.
-- This is just a convenience function that means the same as @'quickCheck' . 'verbose'@.
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

test :: State -> (QCGen -> Int -> Prop) -> IO Result
test st f
  | numSuccessTests st   >= maxSuccessTests st =
    doneTesting st f
  | numDiscardedTests st >= maxDiscardedRatio st * maxSuccessTests st =
    giveUp st f
  | otherwise =
    runATest st f

doneTesting :: State -> (QCGen -> Int -> Prop) -> IO Result
doneTesting st _f
  | expected st == False = do
      putPart (terminal st)
        ( bold ("*** Failed!")
       ++ " Passed "
       ++ show (numSuccessTests st)
       ++ " tests (expected failure)"
        )
      finished NoExpectedFailure
  | not (null (insufficientlyCovered st)) = do
      putPart (terminal st)
        ( bold ("*** Insufficient coverage after ")
       ++ show (numSuccessTests st)
       ++ " tests"
        )
      finished InsufficientCoverage
  | otherwise = do
      putPart (terminal st)
        ( "+++ OK, passed "
       ++ show (numSuccessTests st)
       ++ " tests"
        )
      finished Success
  where
    finished k = do
      success st
      theOutput <- terminalOutput (terminal st)
      return (k (numSuccessTests st) (S.labels st) (S.classifications st) (S.coverage st) theOutput)

giveUp :: State -> (QCGen -> Int -> Prop) -> IO Result
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
                  , labels   = S.labels st
                  , classifications = S.classifications st
                  , coverage = S.coverage st
                  , output   = theOutput
                  }

runATest :: State -> (QCGen -> Int -> Prop) -> IO Result
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
     let size = computeSize st (numSuccessTests st) (numRecentlyDiscardedTests st)
     MkRose res ts <- protectRose (reduceRose (unProp (f rnd1 size)))
     res <- callbackPostTest st res

     let continue break st' | abort res = break st'
                            | otherwise = test st'
     case res of
       MkResult{ok = Just True, expect = expect, maybeNumTests = mnt} -> -- successful test
         do continue doneTesting
              st{ numSuccessTests           = numSuccessTests st + 1
                , numRecentlyDiscardedTests = 0
                , maxSuccessTests           = fromMaybe (maxSuccessTests st) mnt
                , randomSeed                = rnd2
                , S.labels = Map.insertWith (+) (P.labels res) 1 (S.labels st)
                , S.classifications =
                  Map.unionWith (Map.unionWith (+))
                    (S.classifications st)
                    (Map.fromListWith (Map.unionWith (+))
                     [(x, Map.singleton y 1) | (x, y) <- P.classifications res])
                , S.coverage =
                  Map.union (S.coverage st) (Map.fromList (P.coverage res))
                , expected                  = expect
                } f

       MkResult{ok = Nothing, expect = expect, maybeNumTests = mnt} -> -- discarded test
         do continue giveUp
              st{ numDiscardedTests         = numDiscardedTests st + 1
                , numRecentlyDiscardedTests = numRecentlyDiscardedTests st + 1
                , maxSuccessTests           = fromMaybe (maxSuccessTests st) mnt
                , randomSeed                = rnd2
                , S.labels = Map.insertWith (+) (P.labels res) 1 (S.labels st)
                , S.classifications =
                  Map.unionWith (Map.unionWith (+))
                    (S.classifications st)
                    (Map.fromListWith (Map.unionWith (+))
                     [(x, Map.singleton y 1) | (x, y) <- P.classifications res])
                , S.coverage =
                  Map.union (S.coverage st) (Map.fromList (P.coverage res))
                , expected                  = expect
                } f

       MkResult{ok = Just False} -> -- failed test
         do (numShrinks, totFailed, lastFailed, res) <- foundFailure st res ts
            theOutput <- terminalOutput (terminal st)
            if not (expect res) then
              return Success{ labels = S.labels st,
                              classifications = S.classifications st,
                              coverage = S.coverage st,
                              numTests = numSuccessTests st+1,
                              output = theOutput }
             else do
              testCase <- mapM showCounterexample (P.testCase res)
              return Failure{ usedSeed        = randomSeed st -- correct! (this will be split first)
                            , usedSize        = size
                            , numTests        = numSuccessTests st+1
                            , numShrinks      = numShrinks
                            , numShrinkTries  = totFailed
                            , numShrinkFinal  = lastFailed
                            , output          = theOutput
                            , reason          = P.reason res
                            , theException    = P.theException res
                            , failingTestCase = testCase
                            , features        = P.stamp res
                            }
 where
  (rnd1,rnd2) = split (randomSeed st)

failureSummary :: State -> P.Result -> String
failureSummary st res = fst (failureSummaryAndReason st res)

failureReason :: State -> P.Result -> [String]
failureReason st res = snd (failureSummaryAndReason st res)

failureSummaryAndReason :: State -> P.Result -> (String, [String])
failureSummaryAndReason st res = (summary, full)
  where
    summary =
      header ++
      short 26 (oneLine reason ++ " ") ++
      count True ++ "..."

    full =
      (header ++
       (if isOneLine reason then reason ++ " " else "") ++
       count False ++ ":"):
      if isOneLine reason then [] else lines reason

    reason = P.reason res

    header =
      if expect res then
        bold "*** Failed! "
      else "+++ OK, failed as expected. "

    count full =
      "(after " ++ number (numSuccessTests st+1) "test" ++
      concat [
        " and " ++
        show (numSuccessShrinks st) ++
        concat [ "." ++ show (numTryShrinks st) | showNumTryShrinks ] ++
        " shrink" ++
        (if numSuccessShrinks st == 1 && not showNumTryShrinks then "" else "s")
        | numSuccessShrinks st > 0 || showNumTryShrinks ] ++
      ")"
      where
        showNumTryShrinks = full && numTryShrinks st > 0

-- summary :: State -> Map (Maybe String) Table
-- -- summary st = reverse
-- --            . sortBy (comparing snd)
-- --            . map (\ss -> (head ss, fromIntegral (length ss) * 100 / fromIntegral (numSuccessTests st)))
-- --            . group
-- --            . sort
-- --            $ [ concat (intersperse ", " s')
-- --              | s <- collected st
-- --                -- HACK: don't print out labels that were created by 'cover'.
-- --              , let s' = [ t | t <- Set.toList s, Map.lookup t (S.labels st) == Just 0 ]
-- --              , not (null s')
-- --              ]

-- summary = S.tables

success :: State -> IO ()
success st =
  case allLabels ++ covers of
    [] | null longTables ->
      do putLine (terminal st) "."
    [pt] | null longTables ->
      do putLine (terminal st)
           ( " ("
          ++ dropWhile isSpace pt
          ++ ")."
           )
    cases -> do putLine (terminal st) ":"
                mapM_ (putLine $ terminal st) $
                  cases ++
                  concat ["":xss | xss <- longTables]
 where
  allLabels :: [String]
  allLabels =
    [ formatLabel (numSuccessTests st) True (intercalate ", " labels, fromIntegral n / fromIntegral (numSuccessTests st)) | (labels, n) <- Map.toList (S.labels st), not (null labels)] ++
    lefts tables

  longTables :: [[String]]
  longTables = rights tables

  tables :: [Either String [String]]
  tables = [showTable table m | (table, m) <- Map.toList (S.classifications st)]

  -- allLabels = map (formatLabel (numSuccessTests st) True) (summary st)

  covers :: [String]
  covers = [ ("only " ++ formatLabel (numSuccessTests st) False (l, p) ++ ", not " ++ show reqP ++ "%")
           | (l, reqP, p) <- insufficientlyCovered st ]

showTable :: String -> Map String Int -> Either String [String]
showTable table m
  | all (`elem` ["False", "True"]) (Map.keys m) =
    oneLine (Map.findWithDefault 0 "True" m) table
  | otherwise =
    case Map.toList m of
      [(k, v)] ->
        oneLine v (table ++ " " ++ k)
      kvs ->
        manyLines kvs
  where
    k = sum (Map.elems m)
    oneLine n descr =
      Left (formatLabel k True (descr, 100 * fromIntegral n / fromIntegral k))

    manyLines kvs =
      Right . tabulate . map format .
      -- Descending order of occurrences
      reverse . sortBy (comparing snd) $ kvs
      where
        format (key, v) =
          formatLabel k True (key, 100 * fromIntegral v / fromIntegral k)

    tabulate rows =
      [sep,
       border '|' ' ' table,
       border '|' ' ' total,
       sep] ++
      map (border '|' ' ' . ljust bodywidth) rows ++
      [sep]
      where
        headerwidth = max (length table) (length total)
        bodywidth = maximum (map length rows)
        width = max headerwidth bodywidth

        total = printf "(%d in total)" k
        
        sep = border '+' '-' $ replicate width '-'
        border x y xs = [x, y] ++ centre width xs ++ [y, x]

        ljust n xs = xs ++ replicate (n - length xs) ' '
        rjust n xs = replicate (n - length xs) ' ' ++ xs
        centre n xs =
          ljust n $
          replicate ((n - length xs) `div` 2) ' ' ++ xs

formatLabel :: Int -> Bool -> (String, Double) -> String
formatLabel n pad (x, p) = showP pad p ++ " " ++ x
 where
  showP :: Bool -> Double -> String
  showP pad p =
    (if pad && p < 10 then " " else "") ++
    printf "%.*f" places p ++ "%"

  -- Show no decimal places if <= 100 successful tests,
  -- one decimal place if <= 1000 successful tests,
  -- two decimal places if <= 10000 successful tests, and so on.
  places :: Integer
  places =
    ceiling (logBase 10 (fromIntegral n) - 2 :: Double) `max` 0

labelCount :: String -> State -> Int
labelCount l st =
  -- XXX in case of a disjunction, a label can occur several times,
  -- need to think what to do there
  -- length [ l' | l' <- concat (map Set.toList (collected st)), l == l' ]
  0

percentage :: Integral a => State -> a -> Double
percentage st n =
  fromIntegral n * 100 / fromIntegral (numSuccessTests st)

insufficientlyCovered :: State -> [(String, Double, Double)]
insufficientlyCovered st = []
-- insufficientlyCovered st =
--   [ (l, reqP, p)
--   | (l, reqP) <- Map.toList (S.labels st),
--     let p = percentage st (labelCount l st),
--     p < reqP ]

--------------------------------------------------------------------------
-- main shrinking loop

foundFailure :: State -> P.Result -> [Rose P.Result] -> IO (Int, Int, Int, P.Result)
foundFailure st res ts =
  do localMin st{ numTryShrinks = 0 } res ts

localMin :: State -> P.Result -> [Rose P.Result] -> IO (Int, Int, Int, P.Result)
-- Don't try to shrink for too long
localMin st res ts
  | numSuccessShrinks st + numTotTryShrinks st >= numTotMaxShrinks st =
    localMinFound st res
localMin st res ts = do
  r <- tryEvaluateIO $
    putTemp (terminal st) (failureSummary st res)
  case r of
    Left err ->
      localMinFound st (exception "Exception while printing status message" err) { callbacks = callbacks res }
    Right () -> do
      r <- tryEvaluate ts
      case r of
        Left err ->
          localMinFound st
            (exception "Exception while generating shrink-list" err) { callbacks = callbacks res }
        Right ts' -> localMin' st res ts'

localMin' :: State -> P.Result -> [Rose P.Result] -> IO (Int, Int, Int, P.Result)
localMin' st res [] = localMinFound st res
localMin' st res (t:ts) =
  do -- CALLBACK before_test
    MkRose res' ts' <- protectRose (reduceRose t)
    res' <- callbackPostTest st res'
    if ok res' == Just False
      then localMin st{ numSuccessShrinks = numSuccessShrinks st + 1,
                        numTryShrinks     = 0 } res' ts'
      else localMin st{ numTryShrinks    = numTryShrinks st + 1,
                        numTotTryShrinks = numTotTryShrinks st + 1 } res ts

localMinFound :: State -> P.Result -> IO (Int, Int, Int, P.Result)
localMinFound st res =
  do sequence_ [ putLine (terminal st) msg | msg <- failureReason st res ]
     callbackPostFinalFailure st res
     -- NB no need to check if callbacks threw an exception because
     -- we are about to return to the user anyway
     return (numSuccessShrinks st, numTotTryShrinks st - numTryShrinks st, numTryShrinks st, res)

--------------------------------------------------------------------------
-- callbacks

callbackPostTest :: State -> P.Result -> IO P.Result
callbackPostTest st res = protect (exception "Exception running callback") $ do
  sequence_ [ f st res | PostTest _ f <- callbacks res ]
  return res

callbackPostFinalFailure :: State -> P.Result -> IO ()
callbackPostFinalFailure st res = do
  x <- tryEvaluateIO $ sequence_ [ f st res | PostFinalFailure _ f <- callbacks res ]
  case x of
    Left err -> do
      putLine (terminal st) "*** Exception running callback: "
      tryEvaluateIO $ putLine (terminal st) (show err)
      return ()
    Right () -> return ()

--------------------------------------------------------------------------
-- the end.
