{-# OPTIONS_HADDOCK hide #-}
-- | The main test loop.
{-# LANGUAGE CPP #-}
#ifndef NO_TYPEABLE
{-# LANGUAGE DeriveDataTypeable #-}
#endif
#ifndef NO_SAFE_HASKELL
{-# LANGUAGE Trustworthy #-}
#endif
module Test.QuickCheck.Test where

--------------------------------------------------------------------------
-- imports

import Control.Applicative
import Test.QuickCheck.Gen
import Test.QuickCheck.Property hiding ( Result( reason, theException, labels, classes, tables ), (.&.) )
import qualified Test.QuickCheck.Property as P
import Test.QuickCheck.Text
import Test.QuickCheck.State hiding (labels, classes, tables, requiredCoverage)
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
  )

import Data.Maybe(fromMaybe, isNothing, catMaybes)
import Data.Ord(comparing)
import Text.Printf(printf)
import Control.Monad
import Data.Bits

#ifndef NO_TYPEABLE
import Data.Typeable (Typeable)
#endif

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
 deriving ( Show, Read
#ifndef NO_TYPEABLE
  , Typeable
#endif
  )

-- | Result represents the test result
data Result
  -- | A successful test run
  = Success
    { numTests     :: Int
      -- ^ Number of tests performed
    , numDiscarded :: Int
      -- ^ Number of tests skipped
    , labels       :: !(Map [String] Int)
      -- ^ The number of test cases having each combination of labels (see 'label')
    , classes      :: !(Map String Int)
      -- ^ The number of test cases having each class (see 'classify')
    , tables       :: !(Map String (Map String Int))
      -- ^ Data collected by 'tabulate'
    , output       :: String
      -- ^ Printed output
    }
  -- | Given up
  | GaveUp
    { numTests     :: Int
    , numDiscarded :: Int
      -- ^ Number of tests skipped
    , labels       :: !(Map [String] Int)
    , classes      :: !(Map String Int)
    , tables       :: !(Map String (Map String Int))
    , output       :: String
    }
  -- | A failed test run
  | Failure
    { numTests        :: Int
    , numDiscarded    :: Int
      -- ^ Number of tests skipped
    , numShrinks      :: Int
      -- ^ Number of successful shrinking steps performed
    , numShrinkTries  :: Int
      -- ^ Number of unsuccessful shrinking steps performed
    , numShrinkFinal  :: Int
      -- ^ Number of unsuccessful shrinking steps performed since last successful shrink
    , usedSeed        :: QCGen
      -- ^ What seed was used
    , usedSize        :: Int
      -- ^ What was the test size
    , reason          :: String
      -- ^ Why did the property fail
    , theException    :: Maybe AnException
      -- ^ The exception the property threw, if any
    , output          :: String
    , failingTestCase :: [String]
      -- ^ The test case which provoked the failure
    , failingLabels   :: [String]
      -- ^ The test case's labels (see 'label')
    , failingClasses  :: Set String
      -- ^ The test case's classes (see 'classify')
    }
  -- | A property that should have failed did not
  | NoExpectedFailure
    { numTests     :: Int
    , numDiscarded :: Int
      -- ^ Number of tests skipped
    , labels       :: !(Map [String] Int)
    , classes      :: !(Map String Int)
    , tables       :: !(Map String (Map String Int))
    , output       :: String
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
--
-- If you want to get the counterexample as a Haskell value,
-- rather than just printing it, try the
-- <http://hackage.haskell.org/package/quickcheck-with-counterexamples quickcheck-with-counterexamples>
-- package.

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
  withState a (\s -> test s (property p))

withState :: Args -> (State -> IO a) -> IO a
withState a test = (if chatty a then withStdioTerminal else withNullTerminal) $ \tm -> do
     rnd <- case replay a of
              Nothing      -> newQCGen
              Just (rnd,_) -> return rnd
     test MkState{ terminal                  = tm
                 , maxSuccessTests           = maxSuccess a
                 , coverageConfidence        = Nothing
                 , maxDiscardedRatio         = maxDiscardRatio a
                 , replayStartSize           = snd <$> replay a
                 , maxTestSize               = maxSize a
                 , numTotMaxShrinks          = maxShrinks a
                 , numSuccessTests           = 0
                 , numDiscardedTests         = 0
                 , numRecentlyDiscardedTests = 0
                 , S.labels                  = Map.empty
                 , S.classes                 = Map.empty
                 , S.tables                  = Map.empty
                 , S.requiredCoverage        = Map.empty
                 , expected                  = True
                 , randomSeed                = rnd
                 , numSuccessShrinks         = 0
                 , numTryShrinks             = 0
                 , numTotTryShrinks          = 0
                 }

computeSize :: State -> Int
computeSize MkState{replayStartSize = Just s,numSuccessTests = 0,numRecentlyDiscardedTests=0} = s
computeSize MkState{maxSuccessTests = ms, maxTestSize = mts, maxDiscardedRatio = md,numSuccessTests=n,numRecentlyDiscardedTests=d}
    -- e.g. with maxSuccess = 250, maxSize = 100, goes like this:
    -- 0, 1, 2, ..., 99, 0, 1, 2, ..., 99, 0, 2, 4, ..., 98.
    | n `roundTo` mts + mts <= ms ||
      n >= ms ||
      ms `mod` mts == 0 = (n `mod` mts + d `div` dDenom) `min` mts
    | otherwise =
      ((n `mod` mts) * mts `div` (ms `mod` mts) + d `div` dDenom) `min` mts
  where
    -- The inverse of the rate at which we increase size as a function of discarded tests
    -- if the discard ratio is high we can afford this to be slow, but if the discard ratio
    -- is low we risk bowing out too early
    dDenom
      | md > 0 = (ms * md `div` 3) `clamp` (1, 10)
      | otherwise = 1 -- Doesn't matter because there will be no discards allowed
    n `roundTo` m = (n `div` m) * m

clamp :: Ord a => a -> (a, a) -> a
clamp x (l, h) = max l (min x h)

-- | Tests a property and prints the results and all test cases generated to 'stdout'.
-- This is just a convenience function that means the same as @'quickCheck' . 'verbose'@.
--
-- Note: for technical reasons, the test case is printed out /after/
-- the property is tested. To debug a property that goes into an
-- infinite loop, use 'within' to add a timeout instead.
verboseCheck :: Testable prop => prop -> IO ()
verboseCheck p = quickCheck (verbose p)

-- | Tests a property, using test arguments, and prints the results and all test cases generated to 'stdout'.
-- This is just a convenience function that combines 'quickCheckWith' and 'verbose'.
--
-- Note: for technical reasons, the test case is printed out /after/
-- the property is tested. To debug a property that goes into an
-- infinite loop, use 'within' to add a timeout instead.
verboseCheckWith :: Testable prop => Args -> prop -> IO ()
verboseCheckWith args p = quickCheckWith args (verbose p)

-- | Tests a property, produces a test result, and prints the results and all test cases generated to 'stdout'.
-- This is just a convenience function that combines 'quickCheckResult' and 'verbose'.
--
-- Note: for technical reasons, the test case is printed out /after/
-- the property is tested. To debug a property that goes into an
-- infinite loop, use 'within' to add a timeout instead.
verboseCheckResult :: Testable prop => prop -> IO Result
verboseCheckResult p = quickCheckResult (verbose p)

-- | Tests a property, using test arguments, produces a test result, and prints the results and all test cases generated to 'stdout'.
-- This is just a convenience function that combines 'quickCheckWithResult' and 'verbose'.
--
-- Note: for technical reasons, the test case is printed out /after/
-- the property is tested. To debug a property that goes into an
-- infinite loop, use 'within' to add a timeout instead.
verboseCheckWithResult :: Testable prop => Args -> prop -> IO Result
verboseCheckWithResult a p = quickCheckWithResult a (verbose p)

--------------------------------------------------------------------------
-- main test loop

test :: State -> Property -> IO Result
test st f
  | numSuccessTests st   >= maxSuccessTests st && isNothing (coverageConfidence st) =
    doneTesting st f
  | numDiscardedTests st >= maxDiscardedRatio st * max (numSuccessTests st) (maxSuccessTests st) =
    giveUp st f
  | otherwise =
    runATest st f

doneTesting :: State -> Property -> IO Result
doneTesting st _f
  | expected st == False = do
      putPart (terminal st)
        ( bold ("*** Failed!")
       ++ " Passed "
       ++ showTestCount st
       ++ " (expected failure)"
        )
      finished NoExpectedFailure
  | otherwise = do
      putPart (terminal st)
        ( "+++ OK, passed "
       ++ showTestCount st
        )
      finished Success
  where
    finished k = do
      success st
      theOutput <- terminalOutput (terminal st)
      return (k (numSuccessTests st) (numDiscardedTests st) (S.labels st) (S.classes st) (S.tables st) theOutput)

giveUp :: State -> Property -> IO Result
giveUp st _f =
  do -- CALLBACK gave_up?
     putPart (terminal st)
       ( bold ("*** Gave up!")
      ++ " Passed only "
      ++ showTestCount st
      ++ " tests"
       )
     success st
     theOutput <- terminalOutput (terminal st)
     return GaveUp{ numTests     = numSuccessTests st
                  , numDiscarded = numDiscardedTests st
                  , labels       = S.labels st
                  , classes      = S.classes st
                  , tables       = S.tables st
                  , output       = theOutput
                  }

showTestCount :: State -> String
showTestCount st =
     number (numSuccessTests st) "test"
  ++ concat [ "; " ++ show (numDiscardedTests st) ++ " discarded"
            | numDiscardedTests st > 0
            ]

runATest :: State -> Property -> IO Result
runATest st f =
  do -- CALLBACK before_test
     putTemp (terminal st)
        ( "("
       ++ showTestCount st
       ++ ")"
        )
     let powerOfTwo n = n .&. (n - 1) == 0
     let f_or_cov =
           case coverageConfidence st of
             Just confidence | (1 + numSuccessTests st) `mod` 100 == 0 && powerOfTwo ((1 + numSuccessTests st) `div` 100) ->
               addCoverageCheck confidence st f
             _ -> f
     let size = computeSize st
     MkRose res ts <- protectRose (reduceRose (unProp (unGen (unProperty f_or_cov) rnd1 size)))
     res <- callbackPostTest st res

     let continue break st' | abort res = break st'
                            | otherwise = test st'

     let st' = st{ coverageConfidence = maybeCheckCoverage res `mplus` coverageConfidence st
                 , maxSuccessTests = fromMaybe (maxSuccessTests st) (maybeNumTests res)
                 , maxDiscardedRatio = fromMaybe (maxDiscardedRatio st) (maybeDiscardedRatio res)
                 , S.labels = Map.insertWith (+) (P.labels res) 1 (S.labels st)
                 , S.classes = Map.unionWith (+) (S.classes st) (Map.fromList (zip (P.classes res) (repeat 1)))
                 , S.tables =
                   foldr (\(tab, x) -> Map.insertWith (Map.unionWith (+)) tab (Map.singleton x 1))
                     (S.tables st) (P.tables res)
                 , S.requiredCoverage =
                   foldr (\(key, value, p) -> Map.insertWith max (key, value) p)
                     (S.requiredCoverage st) (P.requiredCoverage res)
                 , expected = expect res }

     case res of
       MkResult{ok = Just True} -> -- successful test
         do continue doneTesting
              st'{ numSuccessTests           = numSuccessTests st' + 1
                 , numRecentlyDiscardedTests = 0
                 , randomSeed = rnd2
                 } f

       MkResult{ok = Nothing} -> -- discarded test
         do continue giveUp
              -- Don't add coverage info from this test
              st{ numDiscardedTests         = numDiscardedTests st' + 1
                , numRecentlyDiscardedTests = numRecentlyDiscardedTests st' + 1
                , maxSuccessTests           = fromMaybe (maxSuccessTests st) (maybeNumTests res)
                , maxDiscardedRatio         = fromMaybe (maxDiscardedRatio st) (maybeDiscardedRatio res)
                , randomSeed                = rnd2
                } f

       MkResult{ok = Just False} -> -- failed test
         do (numShrinks, totFailed, lastFailed, res) <- foundFailure st' res ts
            theOutput <- terminalOutput (terminal st')
            if not (expect res) then
              return Success{ labels = S.labels st',
                              classes = S.classes st',
                              tables = S.tables st',
                              numTests = numSuccessTests st'+1,
                              numDiscarded = numDiscardedTests st',
                              output = theOutput }
             else do
              testCase <- mapM showCounterexample (P.testCase res)
              return Failure{ usedSeed        = randomSeed st' -- correct! (this will be split first)
                            , usedSize        = size
                            , numTests        = numSuccessTests st'+1
                            , numDiscarded    = numDiscardedTests st'
                            , numShrinks      = numShrinks
                            , numShrinkTries  = totFailed
                            , numShrinkFinal  = lastFailed
                            , output          = theOutput
                            , reason          = P.reason res
                            , theException    = P.theException res
                            , failingTestCase = testCase
                            , failingLabels   = P.labels res
                            , failingClasses  = Set.fromList (P.classes res)
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
      short 26 (oneLine theReason ++ " ") ++
      count True ++ "..."

    full =
      (header ++
       (if isOneLine theReason then theReason ++ " " else "") ++
       count False ++ ":"):
      if isOneLine theReason then [] else lines theReason

    theReason = P.reason res

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

success :: State -> IO ()
success st = do
  mapM_ (putLine $ terminal st) (paragraphs [short, long])
  where
    (short, long) =
      case labelsAndTables st of
        ([msg], long) ->
          ([" (" ++ dropWhile isSpace msg ++ ")."], long)
        ([], long) ->
          (["."], long)
        (short, long) ->
          (":":short, long)

labelsAndTables :: State -> ([String], [String])
labelsAndTables st = (theLabels, theTables)
  where
    theLabels :: [String]
    theLabels =
      paragraphs $
        [ showTable (numSuccessTests st) Nothing m
        | m <- S.classes st:Map.elems numberedLabels ]

    numberedLabels :: Map Int (Map String Int)
    numberedLabels =
      Map.fromListWith (Map.unionWith (+)) $
        [ (i, Map.singleton l n)
        | (labels, n) <- Map.toList (S.labels st),
          (i, l) <- zip [0..] labels ]

    theTables :: [String]
    theTables =
      paragraphs $
        [ showTable (sum (Map.elems m)) (Just table) m
        | (table, m) <- Map.toList (S.tables st) ] ++
        [[ (case mtable of Nothing -> "Only "; Just table -> "Table '" ++ table ++ "' had only ")
         ++ lpercent n tot ++ " " ++ label ++ ", but expected " ++ lpercentage p tot
         | (mtable, label, tot, n, p) <- allCoverage st,
           insufficientlyCovered (fmap certainty (coverageConfidence st)) tot n p ]]

showTable :: Int -> Maybe String -> Map String Int -> [String]
showTable k mtable m =
  [table ++ " " ++ total ++ ":" | Just table <- [mtable]] ++
  (map format .
   -- Descending order of occurrences
   reverse . sortBy (comparing snd) .
   -- If #occurences the same, sort in increasing order of key
   -- (note: works because sortBy is stable)
   reverse . sortBy (comparing fst) $ Map.toList m)
  where
    format (key, v) =
      rpercent v k ++ " " ++ key

    total = printf "(%d in total)" k

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

----------------------------------------------------------------------
-- computing coverage

sufficientlyCovered :: Confidence -> Int -> Int -> Double -> Bool
sufficientlyCovered confidence n k p =
  -- Accept the coverage if, with high confidence, the actual probability is
  -- at least 0.9 times the required one.
  wilsonLow (fromIntegral k) (fromIntegral n) (1 / fromIntegral err) >= tol * p
  where
    err = certainty confidence
    tol = tolerance confidence

insufficientlyCovered :: Maybe Integer -> Int -> Int -> Double -> Bool
insufficientlyCovered Nothing n k p =
  fromIntegral k < p * fromIntegral n
insufficientlyCovered (Just err) n k p =
  wilsonHigh (fromIntegral k) (fromIntegral n) (1 / fromIntegral err) < p

-- https://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval#Wilson_score_interval
-- Note:
-- https://www.ncss.com/wp-content/themes/ncss/pdf/Procedures/PASS/Confidence_Intervals_for_One_Proportion.pdf
-- suggests we should use a instead of a/2 for a one-sided test. Look
-- into this.
wilson :: Integer -> Integer -> Double -> Double
wilson k n z =
  (p + z*z/(2*nf) + z*sqrt (p*(1-p)/nf + z*z/(4*nf*nf)))/(1 + z*z/nf)
  where
    nf = fromIntegral n
    p = fromIntegral k / fromIntegral n

wilsonLow :: Integer -> Integer -> Double -> Double
wilsonLow k n a = wilson k n (invnormcdf (a/2))

wilsonHigh :: Integer -> Integer -> Double -> Double
wilsonHigh k n a = wilson k n (invnormcdf (1-a/2))

-- Algorithm taken from
-- https://web.archive.org/web/20151110174102/http://home.online.no/~pjacklam/notes/invnorm/
-- Accurate to about one part in 10^9.
--
-- The 'erf' package uses the same algorithm, but with an extra step
-- to get a fully accurate result, which we skip because it requires
-- the 'erfc' function.
invnormcdf :: Double -> Double
invnormcdf p
  | p < 0  = 0/0
  | p > 1  = 0/0
  | p == 0 = -1/0
  | p == 1 = 1/0
  | p < p_low =
    let
      q = sqrt(-2*log(p))
    in
      (((((c1*q+c2)*q+c3)*q+c4)*q+c5)*q+c6) /
      ((((d1*q+d2)*q+d3)*q+d4)*q+1)
  | p <= p_high =
    let
      q = p - 0.5
      r = q*q
    in
      (((((a1*r+a2)*r+a3)*r+a4)*r+a5)*r+a6)*q /
      (((((b1*r+b2)*r+b3)*r+b4)*r+b5)*r+1)
  | otherwise =
    let
      q = sqrt(-2*log(1-p))
    in
      -(((((c1*q+c2)*q+c3)*q+c4)*q+c5)*q+c6) /
       ((((d1*q+d2)*q+d3)*q+d4)*q+1)
  where
    a1 = -3.969683028665376e+01
    a2 =  2.209460984245205e+02
    a3 = -2.759285104469687e+02
    a4 =  1.383577518672690e+02
    a5 = -3.066479806614716e+01
    a6 =  2.506628277459239e+00

    b1 = -5.447609879822406e+01
    b2 =  1.615858368580409e+02
    b3 = -1.556989798598866e+02
    b4 =  6.680131188771972e+01
    b5 = -1.328068155288572e+01

    c1 = -7.784894002430293e-03
    c2 = -3.223964580411365e-01
    c3 = -2.400758277161838e+00
    c4 = -2.549732539343734e+00
    c5 =  4.374664141464968e+00
    c6 =  2.938163982698783e+00

    d1 =  7.784695709041462e-03
    d2 =  3.224671290700398e-01
    d3 =  2.445134137142996e+00
    d4 =  3.754408661907416e+00

    p_low  = 0.02425
    p_high = 1 - p_low

addCoverageCheck :: Confidence -> State -> Property -> Property
addCoverageCheck confidence st prop
  | and [ sufficientlyCovered confidence tot n p
        | (_, _, tot, n, p) <- allCoverage st ] =
    -- Note: run prop once more so that we get labels for this test case run
    once prop
  | or [ insufficientlyCovered (Just (certainty confidence)) tot n p
       | (_, _, tot, n, p) <- allCoverage st ] =
    let (theLabels, theTables) = labelsAndTables st in
    foldr counterexample (property failed{P.reason = "Insufficient coverage"})
      (paragraphs [theLabels, theTables])
  | otherwise = prop

allCoverage :: State -> [(Maybe String, String, Int, Int, Double)]
allCoverage st =
  [ (key, value, tot, n, p)
  | ((key, value), p) <- Map.toList (S.requiredCoverage st),
    let tot =
          case key of
            Just key -> Map.findWithDefault 0 key totals
            Nothing -> numSuccessTests st,
    let n = Map.findWithDefault 0 value (Map.findWithDefault Map.empty key combinedCounts) ]
  where
    combinedCounts :: Map (Maybe String) (Map String Int)
    combinedCounts =
      Map.insert Nothing (S.classes st)
        (Map.mapKeys Just (S.tables st))

    totals :: Map String Int
    totals = fmap (sum . Map.elems) (S.tables st)

--------------------------------------------------------------------------
-- the end.
