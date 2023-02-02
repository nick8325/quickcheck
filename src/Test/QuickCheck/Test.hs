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

import Test.QuickCheck.Gen
import Test.QuickCheck.Property hiding ( Result( reason, theException, labels, classes, tables ), (.&.) )
import qualified Test.QuickCheck.Property as P
import Test.QuickCheck.Text
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
import Data.IORef

import Data.Char
  ( isSpace
  )

import Data.List
  ( sort
  , sortBy
  , group
  , intersperse
  , intercalate
  , zip3
  , zip4
  , zip5
  , zip6
  , partition
  )

import Data.Maybe(fromMaybe, isNothing, isJust, catMaybes)
import Data.Ord(comparing)
import Text.Printf(printf)
import Control.Monad
import Data.Bits

#ifndef NO_TYPEABLE
import Data.Typeable (Typeable)
#endif

import Control.Concurrent
import Control.Exception
import Control.Monad.Fix

--------------------------------------------------------------------------
-- quickCheck

-- * Running tests

-- | Args specifies arguments to the QuickCheck driver
data Args
  = Args
  { replay           :: Maybe (QCGen,Int)
    -- ^ Should we replay a previous test?
    -- Note: saving a seed from one version of QuickCheck and
    -- replaying it in another is not supported.
    -- If you want to store a test case permanently you should save
    -- the test case itself.
  , maxSuccess       :: Int
    -- ^ Maximum number of successful tests before succeeding. Testing stops
    -- at the first failure. If all tests are passing and you want to run more tests,
    -- increase this number.
  , maxDiscardRatio  :: Int
    -- ^ Maximum number of discarded tests per successful test before giving up
  , maxSize          :: Int
    -- ^ Size to use for the biggest test cases
  , chatty           :: Bool
    -- ^ Whether to print anything
  , maxShrinks       :: Int
    -- ^ Maximum number of shrinks to before giving up. Setting this to zero
    --   turns shrinking off.
  , numTesters       :: Int
    -- ^ How many concurrent testers to run (uses @forkIO@ internally). A good number to
    --   use is as many as you have physical cores. Hyperthreading does not seem to add
    --   much value.
  , sizeStrategy     :: SizeStrategy
    -- ^ How to compute the number of successful tests so far to use when computing the
    --   size for a test.
  , rightToWorkSteal :: Bool
    -- ^ Should the testers try to steal the right to run more tests from each other if
    --   they run out?
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
  | Aborted
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

isFailure :: Result -> Bool
isFailure Failure{} = True
isFailure _         = False

isGaveUp :: Result -> Bool
isGaveUp GaveUp{} = True
isGaveUp _        = False

isAborted :: Result -> Bool
isAborted Aborted{} = True
isAborted _         = False

isNoExpectedFailure :: Result -> Bool
isNoExpectedFailure NoExpectedFailure{} = True
isNoExpectedFailure _                   = False

-- | The default test arguments
stdArgs :: Args
stdArgs = Args
  { replay           = Nothing
  , maxSuccess       = 100
  , maxDiscardRatio  = 10
  , maxSize          = 100
  , chatty           = True
  , maxShrinks       = maxBound
  , numTesters       = 1
  , sizeStrategy     = Offset
  , rightToWorkSteal = True
  }

-- -- | @ParallelArgs@ specify the internal testing loops parallel behavior
-- data ParallelArgs
--   = ParallelArgs
--   { numTesters :: Int
--   {- ^ How many concurrent testers to run (uses @forkIO@ internally). A good number to
--   use is as many as you have physical cores. Hyperthreading does not seem to add
--   much value. -}
--   , sizeStrategy :: SizeStrategy
--   {- ^ How to compute the number of successful tests so far to use when computing the
--   size for a test. -}
--   , rightToWorkSteal :: Bool
--   {- ^ Should the testers try to steal the right to run more tests from each other if
--   they run out? -}
--   }

{- | The dafault parallel test arguments. By default, the parallel arguments specify that
one HEC should be used, and that the size strategy is @Stride@. The size strategy does not
matter, as using one core makes them behave the same. Right-to-work-stealing is set to
@True@, but has no effect in the precense of only one thread. -}
-- stdParArgs :: ParallelArgs
-- stdParArgs = ParallelArgs
--   { numTesters = 1
--   , sizeStrategy = Stride
--   , rightToWorkSteal = True
--   }

quickCheckPar' :: (Int -> IO a) -> IO a
quickCheckPar' test = do
  numHECs <- getNumCapabilities
  if numHECs == 1
    then do putStrLn "donkey"
            test numHECs
    else test numHECs

{- | Run a property in parallel. This is done by distributing the total number of tests
over all available HECs. If only one HEC is available, it reverts to the sequential
testing framework. -}
quickCheckPar :: Testable prop => prop -> IO ()
quickCheckPar p = quickCheckPar' $ \numhecs ->
  quickCheckInternal (stdArgs { numTesters = numhecs }) p >> return ()
  -- do
  -- numHecs <- getNumCapabilities
  -- if numHecs == 1
  --   then do putStrLn $ concat [ "quickCheckPar called, but only one HEC available -- "
  --                             , "testing will be sequential..."
  --                             ]
  --           quickCheck p
  --   else quickCheckInternal (stdArgs { numTesters = numHECs }) p >> return ()

-- | The parallel version of `quickCheckWith`
quickCheckParWith :: Testable prop => Args -> prop -> IO ()
quickCheckParWith a p = quickCheckPar' $ \numhecs ->
  quickCheckInternal (a { numTesters = numhecs }) p >> return ()
  --quickCheckInternal a pa p >> return ()

-- -- | The parallel version of `quickCheckResult`
quickCheckParResult :: Testable prop => prop -> IO Result
quickCheckParResult p = quickCheckPar' $ \numhecs ->
  quickCheckInternal (stdArgs { numTesters = numhecs }) p
-- quickCheckParResult p = quickCheckInternal stdArgs stdParArgs p

-- -- | The parallel version of `quickCheckWithResult`
quickCheckParWithResult :: Testable prop => Args -> prop -> IO Result
quickCheckParWithResult a p = quickCheckPar' $ \numhecs ->
  quickCheckInternal (a { numTesters = numhecs }) p
  --quickCheckInternal a pa p

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
quickCheckWith args p = quickCheckInternal args p >> return ()

-- | Tests a property, produces a test result, and prints the results to 'stdout'.
quickCheckResult :: Testable prop => prop -> IO Result
quickCheckResult p = quickCheckInternal stdArgs p

-- | Tests a property, produces a test result, and prints the results to 'stdout'.
quickCheckWithResult :: Testable prop => Args -> prop -> IO Result
quickCheckWithResult args p = quickCheckInternal args p

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

-- new testloop v2

--State -> QCGen -> Rose P.Result -> Int
data TesterSignal
  = KillTesters ThreadId State QCGen P.Result [Rose P.Result] Int
  {- ^ Send a kill-signal to all testers, except for the threadID here (this is the id
  of the tester who made the request). -}
  | FinishedTesting
  {- ^ Testing went well, and the main loop should try to fetch all reports without
  sending any kill signal. -}
  | NoMoreDiscardBudget ThreadId
  {- ^ One tester detected that all discard budget was out, and the concurrent testers
  should be force killed. -}
  | Interrupted
  {- ^ This is only returned if the user presses CTRL-C. The UserInterrupt signal is intercepted, and
  this constructor is given to the main thread, at which point it will abort everything. -}

-- | Tests a property, using test arguments, produces a test result, and prints the results to 'stdout'.
quickCheckInternal :: Testable prop => Args -> prop -> IO Result
quickCheckInternal a p = do
      -- either reuse the supplied seed, or generate a new one
      rnd <- case replay a of
               Nothing      -> newQCGen
               Just (rnd,_) -> return rnd

      {- initial seeds for each tester. The seed will be split like this:
      
                     rnd
                    /   \
                   r1    _
                  /     / \
                 r2    _    _
                /     / \  / \
               r3    _   __   _
              /
            ...
      The initial seeds for each tester will be [rnd,r1,r2,r3,...].
            -}
      let initialSeeds = snd $ foldr (\_ (rnd, a) -> let (r1,r2) = split rnd
                                                     in (r1, a ++ [rnd]))
                                     (rnd, [])
                                     [0..numTesters a - 1]

      -- how big to make each testers buffer
      let numTestsPerTester = maxSuccess a `div` numTesters a

      -- number of tests and numsuccessoffset for each tester to use
      let testsoffsetsanddiscards = snd $
            foldr (\_ ((numtests, offset, numdiscards), acc) ->
                      ((numTestsPerTester, offset+numtests, numTestsPerTester * maxDiscardRatio a), acc ++ [(numtests, offset, numdiscards)]))
                  (( numTestsPerTester + (maxSuccess a `rem` numTesters a)
                   , 0
                   , numTestsPerTester * maxDiscardRatio a + ((maxSuccess a `rem` numTesters a) * maxDiscardRatio a) 
                   ), [])
                  [0..numTesters a - 1]

      -- the MVars that holds the test budget for each tester
      testbudgets <- sequence $ replicate (numTesters a) (newIORef 0)

      -- the MVars that hold each testers discard budget
      budgets <- sequence $ replicate (numTesters a) (newIORef 0)

      -- the MVars that hold each testers state
      states <- sequence $ replicate (numTesters a) newEmptyMVar

      -- the components making up a tester
      let testerinfo = zip6 states initialSeeds [0..numTesters a - 1] testbudgets budgets testsoffsetsanddiscards
        
          -- this function tries to steal budget from an MVar Int, if any budget remains.
          -- used for stealing test budgets and discard budgets.
          tryStealBudget [] = return Nothing
          tryStealBudget (b:bs) = do
            v <- claimMoreBudget b 1
            case v of
              Nothing -> tryStealBudget bs
              Just n  -> return $ Just n

      -- parent thread will block on this mvar. When it is unblocked, testing should terminate
      signal <- newEmptyMVar
      numrunning <- newIORef (numTesters a)

      -- initialize the states of each tester
      flip mapM_ testerinfo $ \(st, seed, testerID, tbudget, dbudget, (numtests, testoffset, numdiscards)) -> do
            mask_ $ (if chatty a then withStdioTerminal else withNullTerminal) $ \tm -> do 
              writeIORef tbudget (numtests - 1)
              writeIORef dbudget (numdiscards - 1)
              putMVar st $ (MkState { terminal           = tm
                                    , maxSuccessTests    = maxSuccess a
                                    , coverageConfidence = Nothing
                                    , maxDiscardedRatio  = maxDiscardRatio a
                                    , computeSize = case replay a of
                                                      Nothing -> computeSize'
                                                      Just (_,s) -> computeSize' `at0` s
                                    , numTotMaxShrinks          = maxShrinks a
                                    , numSuccessTests           = 0
                                    , numDiscardedTests         = 0
                                    , numRecentlyDiscardedTests = 0
                                    , stlabels                = Map.empty
                                    , stclasses               = Map.empty
                                    , sttables                = Map.empty
                                    , strequiredCoverage      = Map.empty
                                    , expected                  = True
                                    , randomSeed                = seed
                                    , numSuccessShrinks         = 0
                                    , numTryShrinks             = 0
                                    , numTotTryShrinks          = 0
           
                                    -- new
                                    , testBudget                = tbudget
                                    , stealTests                = if rightToWorkSteal a
                                                                    then tryStealBudget $ filter ((/=) tbudget) testbudgets
                                                                    else return Nothing
                                    , numConcurrent             = numTesters a
                                    , numSuccessOffset          = testoffset
                                    , discardBudget             = dbudget
                                    , stealDiscards             = if rightToWorkSteal a
                                                                    then tryStealBudget $ filter ((/=) dbudget) budgets
                                                                    else return Nothing
                                    , myId                      = testerID
                                    , signalGaveUp              = myThreadId >>= \id -> tryPutMVar signal (NoMoreDiscardBudget id) >> return()
                                    , signalTerminating         = do b <- atomicModifyIORef' numrunning $ \i -> (i-1, i-1 == 0)
                                                                     if b
                                                                      then tryPutMVar signal FinishedTesting >> return ()
                                                                      else return ()
                                    , signalFailureFound = \st seed res ts size -> do tid <- myThreadId
                                                                                      tryPutMVar signal (KillTesters tid st seed res ts size)
                                                                                      return ()
                                    , shouldUpdateAfterWithMaxSuccess = True
                                    , stsizeStrategy                  = sizeStrategy a
                                    })

      -- continuously print current state
      printerID <- if chatty a then Just <$> forkIO (withBuffering $ printer 200 states) else return Nothing

      -- the IO actions that run the test loops      
      let testers = map (\vst -> testLoop vst True (property p)) states

      -- spawn testers
      tids <- zipWithM (\comp vst -> forkIO comp) testers states

      -- wait for wakeup
      s <- readMVar signal `catch` (\UserInterrupt -> return Interrupted)
      mt <- case s of
        Interrupted -> mapM_ (\tid -> throwTo tid QCInterrupted) tids >> mapM_ killThread tids >> return Nothing
        KillTesters tid st seed res ts size -> do mapM_ (\tid -> throwTo tid QCInterrupted >> killThread tid) (filter ((/=) tid) tids)
                                                  return $ Just tid
        FinishedTesting -> return Nothing
        NoMoreDiscardBudget tid -> do mapM_ killThread (filter ((/=) tid) tids)
                                      return $ Just tid

      -- stop printing current progress
      case printerID of
        Just id -> killThread id
        Nothing -> return ()

      -- get report depending on what happened
      reports <- case s of
        KillTesters tid st seed res ts size -> do
          let abortedvsts = map snd $ filter (\(tid', _) -> tid /= tid') (zip tids states)
          failed <- withBuffering $ shrinkResult (chatty a) st seed (numTesters a) res ts size
          aborted <- mapM (\vst -> readMVar vst >>= abortConcurrent) abortedvsts
          return (failed : aborted)
        NoMoreDiscardBudget tid          -> mapM (\vst -> readMVar vst >>= flip giveUp (property p)) states
        FinishedTesting                  -> mapM (\vst -> readMVar vst >>= flip doneTesting (property p)) states
        Interrupted                      -> mapM (\vst -> readMVar vst >>= abortConcurrent) states

      -- compute the required coverage (if any), and merge the individual tester reports
      sts <- mapM readMVar states
      let completeRequiredCoverage = Map.unionsWith max (map strequiredCoverage sts)
          finalReport              = mergeReports reports

      -- output the final outcome to the terminal, clearing the line before a new print is emitted
      putPart (terminal (head sts)) ""
      printFinal (terminal (head sts)) finalReport sts (coverageConfidence (head sts)) completeRequiredCoverage

      -- finally, return the report!
      return $ mergeReports reports
  where
    -- {- | Function that computes the size to use with every test. It looks a bit
    -- scary but I think we can clear it up with some documentation!
    -- 
    -- Input parameters:
      -- - Number of total successful tests executed to far,
      -- - Number of recently discarded tests
    -- 
    -- The function behaves in two different ways. The first branch is taken if:
      -- - Enough more tests will be run such that we can exhaustively try all sizes
        -- from zero to maxsize
      -- - Number of successful tests is larger than or equal to the maximum number of
        -- tests to run. This is the case if we use e.g withMaxSuccess.
      -- - The maximum number of successful tests to execute is a multiple of the size
    -- 
    -- In this case the size is the minimum of:
      -- - The addition of the remainder of dividing the number of successful tests
        -- with the maxsize, and the number of recently discarded tests divided by 10.
      -- - maximum size
    -- 
    -- Otherwise the size of computed as the minimum of:
      -- - some other unintuitive formula
      -- - maximum size
    -- 
    -- The result of this is that the size will be enumerated from 0 to maxsize-1, and
    -- then repeated until there is less than maxsize tests left to run. In that case
    -- it will skip some values in order to go from 0 to 99. E.g with maxSuccess 250 and
    -- maxSize 100, it enumerates [0..99] ++ [0..99] ++ [0,2..98]
    -- -}
    computeSize' :: Int -> Int -> Int
    computeSize' n d
      | n `roundTo` maxSize a + maxSize a <= maxSuccess a ||
        n >= maxSuccess a                                 ||
        maxSuccess a `mod` maxSize a == 0
        = (n `mod` maxSize a + d `div` 10) `min` maxSize a
      | otherwise =
        ((n `mod` maxSize a) * maxSize a `div` (maxSuccess a `mod` maxSize a) + d `div` 10) `min` maxSize a

    {- | @roundTo n m@ rounds down @n@ to the closest multiple of @m@. E.g
    @@@
    roundTo 10 5 = 10
    roundTo 12 5 = 10
    roundTo 9  5 = 5
    roundTo 16 5 = 15
    @@@
    -}
    roundTo :: Integral a => a -> a -> a
    n `roundTo` m = (n `div` m) * m

    {- | If the last two parameters are both zero, the second argument is returned.
    Otherwise the first argument is applied to the last two parameters. -}
    at0 :: (Num a, Num b, Eq a, Eq b) => (a -> b -> c) -> c -> a -> b -> c
    at0 f s 0 0 = s
    at0 f s n d = f n d

-- | Merge every individual testers report into one report containing the composite information.
mergeReports :: [Result] -> Result
mergeReports rs
  | not (null (filter isFailure rs)) =
      createFailed (filter (not . isFailure) rs) (head (filter isFailure rs))
  | null (filter (not . isSuccess) rs)           = createGeneric rs Success
  | null (filter (not . isGaveUp) rs)            = createGeneric rs GaveUp
  | null (filter (not . isAborted) rs)           = createGeneric rs Aborted
  | null (filter (not . isNoExpectedFailure) rs) = createGeneric rs NoExpectedFailure
  | otherwise = error $ concat ["don't know how to merge reports: ", intercalate "\n" $ map show rs]
  where
    -- | create a Result value by passing in a constructor as a parameter to this function
    createGeneric :: [Result]
                  -> (  Int
                     -> Int
                     -> Map [String] Int
                     -> Map String Int
                     -> Map String (Map String Int)
                     -> String
                     -> Result)
                  -> Result
    createGeneric rs f = f (sum $ map numTests rs)
                           (sum $ map numDiscarded rs)
                           (Map.unionsWith (+) $ map labels rs)
                           (Map.unionsWith (+) $ map classes rs)
                           (Map.unionsWith (Map.unionWith (+)) $ map tables rs)
                           (intercalate "\n" $ map output rs)

    {- | create a Result that indicates a failure happened
    NOTE: in this case, the labels and tables are dropped and not reported to the user. -}
    createFailed :: [Result] -> Result -> Result
    createFailed rs f = f { numTests     = sum $ map numTests (f:rs)
                          , numDiscarded = sum $ map numDiscarded (f:rs)
                          , output       = intercalate "\n" $ map output rs
                          }

{- | Given a ref with more budget (either test budget or discard budget), try to claim
- at most @maxchunk@ from it. -}
claimMoreBudget :: IORef Int -> Int -> IO (Maybe Int)
claimMoreBudget budgetioref maxchunk = do
  atomicModifyIORef' budgetioref $ \budget ->
    if budget <= 0
      then (0, Nothing)
      else let chunk = min budget maxchunk
           in (max 0 (budget - chunk), Just chunk)

{- | Update the state in an mvar with a new state.
NOTE: interrupts are masked during the actual update, so that if updatestate
begins evaluation, it will always be allowed to finish. -}
updateState :: MVar State -> State -> IO ()
updateState vst st = do
  modifyMVar_ vst $ \_ -> return st

{- | Given a state, returns @True@ if another test should be executed, and @False@ if not.
If a specific tester thread has run out of testing 'budget', it will try to steal the
right to run more tests from other testers. -}
runOneMore :: State -> IO Bool
runOneMore st = do
  b <- claimMoreBudget (testBudget st) 1
  case b of
    Just _  -> return True
    Nothing -> do n <- stealTests st
                  case n of
                    Nothing -> return False
                    Just _  -> return True

{- | After a test has been discarded, calling this function will let the tester know if
it should stop trying to satisfy the test predicate, or if it should continue. If it has
run out of testing budget, it will try to steal the right to discard more from other
testers. -}
continueAfterDiscard :: State -> IO Bool
continueAfterDiscard st = do
  b <- claimMoreBudget (discardBudget st) 1
  case b of
    Just _ -> return True
    Nothing -> do n <- stealDiscards st
                  case n of
                    Nothing -> return False
                    Just _ -> return True

{- | The actual testing loop that each tester runs. TODO document more-}
testLoop :: MVar State -> Bool -> Property -> IO ()
testLoop vst False f = do
  st <- readMVar vst
  b <- runOneMore st
  if b
    then testLoop vst True f
    else signalTerminating st
testLoop vst True f = do
  st <- readMVar vst

  let (_,s2) = split (randomSeed st)
      (s1,_) = split s2
      numSuccSize = testSizeInput st
  -- generate and run a test, plus postprocessing of the result (TODO turn this into a function)
  res@(MkRose r ts) <- runTest st f s1 (computeSize st numSuccSize (numRecentlyDiscardedTests st))
  let st'                    = updateStateAfterResult res st
      (classification, st'') = resultOf res st'
      st'''                  = st'' { randomSeed = s2 }
  finst <- maybeUpdateAfterWithMaxSuccess res st'''
  case classification of
    -- test was successful!
    OK | abort r -> updateState vst finst >> signalTerminating finst
    OK -> do
      updateState vst finst
      testLoop vst False f
    -- test was discarded, and we're out of discarded budget
    Discarded | abort r -> updateState vst finst >> signalTerminating finst
    Discarded -> do
      b <- continueAfterDiscard st
      if b
        then updateState vst finst >> testLoop vst True f
        else updateState vst finst >> signalGaveUp finst
    
    -- test failed, and we should abort concurrent testers and start shrinking the result
    Failed ->
      signalFailureFound st'' st'' (randomSeed st) r ts (computeSize st numSuccSize (numRecentlyDiscardedTests st))
  where
    -- | Compute the numSuccess-parameter to feed to the @computeSize@ function
    testSizeInput :: State -> Int
    testSizeInput st = case stsizeStrategy st of
      Offset -> numSuccessOffset st + numSuccessTests st
      Stride -> numSuccessTests st * numConcurrent st + myId st

{- | Printing loop. It will read the current test state from the list of @MVar State@,
and print a summary to the terminal. It will do this every @delay@ microseconds.
NOTE: while the actual print is happening, interruptions are masked. This is so that if
the printer is terminated mid-print, the terminal is in an allowed state. -}
printer :: Int -> [MVar State] -> IO ()
printer delay vsts = do
  mask_ printStuff
  threadDelay delay
  printer delay vsts
  where
    -- | Does the actual compiling of the states and printing it to the terminal
    printStuff :: IO ()
    printStuff = do
      states <- sequence $ map readMVar vsts
      putTemp (terminal (head states)) ( concat ["(", summary states, ")"] )
      where
        summary states =
          number (sum (map numSuccessTests states)) "test" ++
          concat [ ";" ++ show (sum (map numDiscardedTests states)) ++ " discarded"
                 | sum (map numDiscardedTests states) > 0
                 ]

{- | This function inspects the final @Result@ of testing and prints a summary to the
terminal. The parameters to this function are

  1. The terminal to which to print
  2. The final @Result@ value
  3. The coverage confidence, if any
  4. The required coverage, if any (the map might be empty)

If @isFailure r = True@, where @r@ is the final @Result@, this function is a no-op.

-}
printFinal :: Terminal -> Result -> [State] -> Maybe Confidence -> Map.Map (Maybe String, String) Double -> IO ()
printFinal terminal r states coverageConfidence requiredCoverage
  | isSuccess r           = do
    putLine terminal ("+++ OK, passed " ++ testCount (numTests r) (numDiscarded r))
    individualTester
    printTheLabelsAndTables
  | isFailure r           = return ()
  | isGaveUp r            = do
    putLine terminal ( bold ("*** Gave up!") ++ " Passed only " ++ testCount (numTests r) (numDiscarded r) ++ " tests")
    individualTester
    printTheLabelsAndTables
  | isAborted r           = do
    putLine terminal ( bold ("*** Aborted prematurely!") ++ " Passed " ++ testCount (numTests r) (numDiscarded r) ++ " before interrupted")
    individualTester
    printTheLabelsAndTables
  | isNoExpectedFailure r = do
    putLine terminal ( bold ("*** Failed!") ++ " Passed " ++ testCount (numTests r) (numDiscarded r) ++ " (expected failure")
    individualTester
    printTheLabelsAndTables
  where
    -- | print the information collected via labels, tabels, coverage etc
    printTheLabelsAndTables :: IO ()
    printTheLabelsAndTables = do
      mapM_ (putLine terminal) (paragraphs [short, long])
    
    (short,long) = case labelsAndTables (labels r) (classes r) (tables r) requiredCoverage (numTests r) coverageConfidence of
      ([msg], long) -> ([" (" ++ dropWhile isSpace msg ++ ")."], long)
      ([], long)    -> ([], long)
      (short, long) -> (":":short, long)

    -- | Final count of successful tests, and discarded tests, rendered as a string
    testCount :: Int -> Int -> String
    testCount numTests numDiscarded =
      concat [ number numTests "test"
             , if numDiscarded > 0
                 then concat ["; ", show numDiscarded, " discarded"]
                 else ""
             ]
    
    individualTester :: IO ()
    individualTester =
      if length states > 1
        then mapM_ (\st -> putLine terminal $ concat ["  tester ", show (myId st), ": ", testCount (numSuccessTests st) (numDiscardedTests st)]) states
        else return ()

{- | This function will shrink the result of a failed test case (if possible), and then
return a final report. The parameters are

  1. The state associated with this test case
  2. The random seed used to generate the failing test case
  3. The result of running the failing test case
  4. The shrinking candidates
  5. The size fed to the test case

-}
shrinkResult :: Bool -> State -> QCGen -> Int -> P.Result -> [Rose P.Result] -> Int -> IO Result
shrinkResult chatty st rs n res ts size = do
  (numShrinks, totFailed, lastFailed, res) <- foundFailure chatty st n res ts
  theOutput <- terminalOutput (terminal st)
  if not (expect res) then
    return Success{ labels       = stlabels st,
                    classes      = stclasses st,
                    tables       = sttables st,
                    numTests     = numSuccessTests st+1,
                    numDiscarded = numDiscardedTests st,
                    output       = theOutput }
   else do
    testCase <- mapM showCounterexample (P.testCase res)
    return Failure{ usedSeed        = (randomSeed st) --rs
                  , usedSize        = size
                  , numTests        = numSuccessTests st+1
                  , numDiscarded    = numDiscardedTests st
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

{- | Inspect the result of running a test, and return the next action to take as well as
an updated state. The parts of the state that might be updated are

  * @numSuccessTests@
  * @numRecentlyDiscardedTests@
  * @numDiscardedTests@

-}
resultOf :: Rose P.Result -> State -> (TestRes, State)
resultOf (MkRose res _) st
  -- successful test
  | ok res == Just True =
             ( OK
             , st { numSuccessTests           = numSuccessTests st + 1
                  , numRecentlyDiscardedTests = 0
                  }
             )
  -- discarded test
  | ok res == Nothing =
             ( Discarded
             , st { numDiscardedTests         = numDiscardedTests st + 1
                  , numRecentlyDiscardedTests = numRecentlyDiscardedTests st + 1
                  }
             )
  -- failed test
  | ok res == Just False = (Failed, st)

-- | The result of running a test
data TestRes
  = OK
  -- ^ The test was OK (successful)
  | Failed
  -- ^ The test failed
  | Discarded
  -- ^ The test was discarded, as it did not meet one of the preconditions

{- | Some test settings are attached to the property rather than the testing arguments,
and will thus only be visible after running a test. This function takes the @Rose Result@
of running a test and the @State@ associated with that test, and updates the state with
information about such settings. Settings affected are

  * @coverageConfidence@
  * @labels@
  * @classes@
  * @tables@
  * @requiredCoverage@ -- what are the coverage requirements?
  * @expected@ -- should the test fail?

-}
updateStateAfterResult :: Rose P.Result -> State -> State
updateStateAfterResult (MkRose res ts) st =
  st { coverageConfidence = maybeCheckCoverage res `mplus` coverageConfidence st
     , maxSuccessTests    = maybe (maxSuccessTests st) id (maybeNumTests res)
     , stlabels = Map.insertWith (+) (P.labels res) 1 (stlabels st)
     , stclasses = Map.unionWith (+) (stclasses st) (Map.fromList (zip (P.classes res) (repeat 1)))
     , sttables = foldr (\(tab, x) -> Map.insertWith (Map.unionWith (+)) tab (Map.singleton x 1))
                     (sttables st) (P.tables res)
     , strequiredCoverage = foldr (\(key, value, p) -> Map.insertWith max (key, value) p)
                     (strequiredCoverage st) (P.requiredCoverage res)
     , expected = expect res
     }

{- | Attached to a property might be desire to run more tests (@withMaxSuccess@). If
this is the case, this function will detect that and recompute the testing/discard
budget and update them accordingly. It will also set a flag in the state that makes it
so that this stuff is not recomputed again. -}
maybeUpdateAfterWithMaxSuccess :: Rose P.Result -> State -> IO State
maybeUpdateAfterWithMaxSuccess (MkRose res ts) st = do
  case maybeNumTests res of
    Nothing -> return st
    Just num -> case shouldUpdateAfterWithMaxSuccess st of
      False -> return st
      True  -> do
        let numTestsPerTester    =
              if myId st == 0
                then num `div` numConcurrent st + (num `rem` numConcurrent st)
                else num `div` numConcurrent st

            newSuccessOffset     =
              if myId st == 0
                then 0
                else numTestsPerTester * myId st + (num `rem` numConcurrent st)

            numDiscardsPerTester = numTestsPerTester * maxDiscardedRatio st

        atomicModifyIORef' (testBudget st) $ \remainingbudget ->
          let newbudget = numTestsPerTester - 1 in (newbudget, ())
        
        atomicModifyIORef' (discardBudget st) $ \remainingbudget ->
          let newbudget = numDiscardsPerTester - 1 in (newbudget, ())

        return $ st { maxSuccessTests                 = num
                    , numSuccessOffset                = newSuccessOffset
                    , shouldUpdateAfterWithMaxSuccess = False
                    }

-- | Run a test
{- | This function will generate and run a test case! The parameters are:

  1. The current @State@ of the tester responsible for running the test
  2. The property to test
  3. The random seed to use
  4. The size to use

-}
runTest :: State -> Property -> QCGen -> Int -> IO (Rose P.Result)
runTest st f seed size = do
  let f_or_cov = case coverageConfidence st of
                   Just confidence | confidenceTest -> addCoverageCheck
                                                         (stlabels st)
                                                         (stclasses st)
                                                         (sttables st)
                                                         (strequiredCoverage st)
                                                         (numSuccessTests st)
                                                         confidence
                                                         f
                   _                                -> f
  MkRose res ts <- protectRose (reduceRose (unProp (unGen (unProperty f_or_cov) seed size)))
  res <- callbackPostTest st res
  return (MkRose res ts)
  where
    {- | Clever way of checking that a number is a power of two in constant time lol
    
    Explanation:

    Example numbers that are a power of two (in binary)

       1 (1)
      10 (2)
     100 (4)
    1000 (8)

    Corresponding numbers when you subtract 1 from them

       0 (0)
      01 (1)
     011 (3)
    0111 (4)

    Computing the bitwise conjunction of the pairwise numbers yield

    0 (   1 & 0)
    0 (  10 & 01)
    0 ( 100 & 011)
    0 (1000 & 0111)

    -}
    powerOfTwo :: (Integral a, Bits a) => a -> Bool
    powerOfTwo n = n .&. (n - 1) == 0

    -- | TODO: Ask Nick to help me describe what this does
    confidenceTest :: Bool
    confidenceTest = (1 + numSuccessTests st) `mod` 100 == 0 && powerOfTwo ((1 + numSuccessTests st) `div` 100)

{- | If a tester terminates without falsifying a property, this function converts the
testers @State@ to a @Result@ -}
doneTesting :: State -> Property -> IO Result
doneTesting st _f
  | expected st == False = do
      finished NoExpectedFailure
  | otherwise = do
      finished Success
  where
    finished k = do
      theOutput <- terminalOutput (terminal st)
      return (k (numSuccessTests st) (numDiscardedTests st) (stlabels st) (stclasses st) (sttables st) theOutput)

{- | If a tester terminates because it discarded too many test cases, this function
converts the testers @State@ to a @Result@ -}
giveUp :: State -> Property -> IO Result
giveUp st _f = do -- CALLBACK gave_up?
     theOutput <- terminalOutput (terminal st)
     return GaveUp{ numTests     = numSuccessTests st
                  , numDiscarded = numDiscardedTests st
                  , labels       = stlabels st
                  , classes      = stclasses st
                  , tables       = sttables st
                  , output       = theOutput
                  }

{- | If a tester terminates because it was aborted by the parent thread, this function
converts the testers @State@ to a @Result@ -}
abortConcurrent :: State -> IO Result
abortConcurrent st = do
     theOutput <- terminalOutput (terminal st)
     return Aborted{ numTests     = numSuccessTests st
                   , numDiscarded = numDiscardedTests st
                   , labels       = stlabels st
                   , classes      = stclasses st
                   , tables       = sttables st
                   , output       = theOutput
                   }

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

labelsAndTables :: Map.Map [String] Int
                 -> Map.Map String Int
                 -> Map.Map String (Map.Map String Int)
                 -> Map.Map (Maybe String, String) Double
                 -> Int -> Maybe Confidence
                 -> ([String], [String])
labelsAndTables labels classes tables requiredCoverage numTests coverageConfidence = (theLabels, theTables)
  where
    theLabels :: [String]
    theLabels =
      paragraphs $
        [showTable numTests Nothing m
        | m <- classes : Map.elems numberedLabels
        ]

    numberedLabels :: Map.Map Int (Map.Map String Int)
    numberedLabels =
      Map.fromListWith (Map.unionWith (+)) $
        [ (i, Map.singleton l n)
        | (labels, n) <- Map.toList labels
        , (i,l) <- zip [0..] labels
        ]

    theTables :: [String]
    theTables =
      paragraphs $
        [ showTable (sum (Map.elems m)) (Just table) m
        | (table, m) <- Map.toList tables
        ] ++
        [[ (case mtable of Nothing -> "Only"; Just table -> "Table '" ++ table ++ "' had only ")
         ++ lpercent n tot ++ " " ++ label ++ ", but expected " ++ lpercentage p tot
         | (mtable, label, tot, n, p) <- allCoverage classes tables requiredCoverage numTests,
         insufficientlyCovered (fmap certainty coverageConfidence) tot n p ]] -- TODO here

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

foundFailureOld :: State -> Int -> P.Result -> [Rose P.Result] -> IO (Int, Int, Int, P.Result)
foundFailureOld st n res ts =
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
-- new shrinking loop

foundFailure :: Bool -> State -> Int -> P.Result -> [Rose P.Result] -> IO (Int, Int, Int, P.Result)
-- foundFailure chatty st n res ts = do
--   (n1,n2,n3,r) <- foundFailureOld st n res ts
--   putStrLn $ show (n1,n2,n3)
--   return (n1,n2,n3,r)
foundFailure chatty st n res ts = do
  re@(n1,n2,n3,r) <- producer chatty False st n res ts
  return re

producer :: Bool -> Bool -> State -> Int -> P.Result -> [Rose P.Result] -> IO (Int, Int, Int, P.Result)
producer chatty detshrinking st n res ts = do

  jobs       <- newMVar (0, 0, Map.empty, [], res, ts)
  numWorkers <- newIORef 0
  stats      <- newIORef (0,0,0)
  signal     <- newEmptyMVar

  -- continuously print current state
  printerID <- if chatty
                 then Just <$> forkIO (shrinkPrinter (terminal st) stats numWorkers (numSuccessTests st) res 200)
                 else return Nothing

  -- start shrinking
  spawnWorkers n jobs stats numWorkers signal

  -- stop printing
  case printerID of
    Just tid -> killThread tid
    Nothing -> return ()
  
  -- need to block here until completely done, somehow
  takeMVar signal

  -- get res
  r <- (\(_,_,_,p,r,_) -> r) <$> readMVar jobs
  (ns,nt,ntot) <- readIORef stats

  return (ns, ntot-nt, nt, r)
  where
    worker :: MVar (Int, Int, Map.Map ThreadId (Int, Int), [(Int, Int)], P.Result, [Rose P.Result])
           -> IORef (Int, Int, Int)
           -> IORef Int
           -> MVar ()
           -> IO ()
    worker jobs stats numFinished signal = do
      j <- getJob jobs
      case j of
        Nothing      -> removeFromMap jobs >> notifyFinished numFinished signal --undefined -- wake up parent
        Just (r,c,t) -> do
          mec <- evaluateCandidate t
          case mec of
            Nothing          -> do
              putStrLn $ concat ["unsuccessful shrink: ", show (r,c)]
              failedShrink stats
              worker jobs stats numFinished signal
            Just (res', ts') -> do
              successShrink stats
              updateWork res' ts' (r,c) jobs stats numFinished signal
              worker jobs stats numFinished signal

    getJob :: MVar (Int, Int, Map.Map ThreadId (Int, Int), [(Int, Int)], P.Result, [Rose P.Result])
           -> IO (Maybe (Int, Int, Rose P.Result))
    getJob jobs = do
      tid <- myThreadId
      modifyMVar jobs $ \(r,c,wm,path,res,xs) ->
        case xs of
          []     -> return ((r,c,wm,path,res,[]), Nothing)
          (t:ts) -> return ((r,c+1,Map.insert tid (r,c) wm,path,res,ts), Just (r, c, t))

    notifyFinished :: IORef Int -> MVar () -> IO ()
    notifyFinished numFinished signal = do
      b <- atomicModifyIORef' numFinished $ \i -> (i + 1, i + 1)
      if b == n
        then putMVar signal ()
        else return ()

    resetFinished :: IORef Int -> IO ()
    resetFinished numFinished = atomicModifyIORef' numFinished $ \_ -> (0, ())

    removeFromMap :: MVar (Int, Int, Map.Map ThreadId (Int, Int), [(Int, Int)], P.Result, [Rose P.Result]) -> IO ()
    removeFromMap jobs = do
      tid <- myThreadId
      modifyMVar jobs $ \(r,c,wm,path,res,xs) -> return ((r,c,Map.delete tid wm, path, res, xs), ())

    evaluateCandidate :: Rose P.Result -> IO (Maybe (P.Result, [Rose P.Result]))
    evaluateCandidate t = do
      MkRose res' ts' <- protectRose (reduceRose t)
      res' <- callbackPostTest st res'
      if ok res' == Just False
        then return $ Just (res', ts')
        else return Nothing

    failedShrink :: IORef (Int, Int, Int) -> IO ()
    failedShrink stats = atomicModifyIORef' stats $ \(ns, nt, ntot) -> ((ns, nt + 1, ntot + 1), ())

    successShrink :: IORef (Int, Int, Int) -> IO ()
    successShrink stats = atomicModifyIORef' stats $ \(ns, nt, ntot) -> ((ns + 1, 0, ntot), ())

    updateWork :: P.Result
               -> [Rose P.Result]
               -> (Int, Int)
               -> MVar (Int, Int, Map.Map ThreadId (Int, Int), [(Int, Int)], P.Result, [Rose P.Result])
               -> IORef (Int, Int, Int)
               -> IORef Int
               -> MVar ()
               -> IO ()
    updateWork res' ts' cand@(r',c') jobs stats numFinished signal = do
      tid <- myThreadId
      if null ts'
        then do modifyMVar jobs $ \(r,c,wm,path,res,ts) -> do
                  let (path', _) = computePath path cand
                      tids = filter (\tid' -> tid /= tid') $ Map.keys wm -- ids of all other workers
                  gracefullyKill tids
                  return ((r'+1,0,Map.empty,path',res',ts'), ())
                putMVar signal ()
        else modifyMVar jobs $ \(r,c,wm,path,res,ts) -> do
               let (path', b)  = computePath path cand
                   (tids, wm') = toKill tid wm path'
               gracefullyKill tids
               spawnWorkers (length tids) jobs stats numFinished signal
               n <- readIORef numFinished
               resetFinished numFinished
               spawnWorkers n jobs stats numFinished signal
               return ((r'+1,0,wm',path',res',ts'), ())
      where
        toKill :: ThreadId
               -> Map.Map ThreadId (Int, Int)
               -> [(Int, Int)]
               -> ([ThreadId], Map.Map ThreadId (Int, Int))
        toKill tid wm path
          | detshrinking = let asList          = Map.toList wm
                               jobsToTerminate = shouldDie (map snd asList) path
                               (tokill, keep)  = partition (\worker -> snd worker `elem` jobsToTerminate) asList
                           in (filter ((/=) tid) (map fst tokill), Map.fromList keep)
          | otherwise    = (filter ((/=) tid) $ Map.keys wm, Map.empty) -- kill everyone

    gracefullyKill :: [ThreadId] -> IO ()
    gracefullyKill tids = mapM_ (\tid -> throwTo tid QCInterrupted >> killThread tid) tids

    spawnWorkers :: Int
                 -> MVar (Int, Int, Map.Map ThreadId (Int, Int), [(Int, Int)], P.Result, [Rose P.Result])
                 -> IORef (Int, Int, Int)
                 -> IORef Int
                 -> MVar ()
                 -> IO ()
    spawnWorkers num jobs stats numFinished signal =
      sequence_ $ replicate num $ forkIO $ defHandler $ worker jobs stats numFinished signal
      where
        defHandler :: IO a -> IO a
        defHandler ioa = ioa `catch` \QCInterrupted -> do tid <- myThreadId
                                                          killThread tid
                                                          error "will never evaluate"

    {- | Given a list of jobs being evaluated, and the taken path, return a list of those
    jobs that should be cancelled. -}
    shouldDie :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
    shouldDie activeWorkers path = flip filter activeWorkers $ \(wr,wc) ->
      -- compute the path that starts at wr
      let path' = dropWhile (\(pr,pc) -> pr > wr) path
      in case path' of
        []          -> False
                    -- are you a candidate 'to the left' of the already chosen one?
        ((sr,sc):_) | sr == wr -> wc > sc
                    | sr < wr  -> True
    
    {- | Given the current path and a new candidate location, check if the new location
    should be part of the path, or if the path should remain unchanged.
    Returns the path to use, and a bool to indicate if it is a different path than the
    one fed into the function.  
    -}
    computePath :: [(Int, Int)] -> (Int, Int) -> ([(Int, Int)], Bool)
    computePath [] (x,y) = ([(x,y)], True)
    computePath ((ox,oy):xs) (x,y)
      | x > ox            = ((x,y)   : (ox,oy) : xs, True)
      | x == ox && y < oy = ((x,y)   :           xs, True)
      | x == ox && y > oy = ((ox,oy) :           xs, False)
      | x < ox            =
          let (xs',b) = computePath xs (x,y)
          in if b
               then (xs', b)
               else ((ox,oy) : xs, b)

shrinkPrinter :: Terminal -> IORef (Int, Int, Int) -> IORef Int -> Int -> P.Result -> Int -> IO ()
shrinkPrinter terminal stats numConcurrent n res delay = do
  triple <- readIORef stats
  numconc <- readIORef numConcurrent
  let output = fst $ failureSummaryAndReason2 triple n numconc res
  withBuffering $ putTemp terminal output
  threadDelay delay
  shrinkPrinter terminal stats numConcurrent n res delay

failureSummaryAndReason2 :: (Int, Int, Int) -> Int -> Int -> P.Result -> (String, [String])
failureSummaryAndReason2 (ns, nt, _) numSuccTests numConc res = (summary, full)
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
      "concurrent testers: " ++ show numConc ++ " " ++
      "(after " ++ number (numSuccTests + 1) "test" ++
      concat [
        " and " ++
        show ns ++
        concat [ "." ++ show nt | showNumTryShrinks ] ++
        " shrink" ++
        (if ns == 1 && not showNumTryShrinks then "" else "s")
        | ns > 0 || showNumTryShrinks ] ++
      ")"
      where
        showNumTryShrinks = full && nt > 0

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

addCoverageCheck :: Map.Map [String] Int
                  -> Map.Map String Int
                  -> Map.Map String (Map.Map String Int)
                  -> Map.Map (Maybe String, String) Double
                  -> Int
                  -> Confidence
                  -> Property
                  -> Property
addCoverageCheck labels classes tables requiredCoverage numTests coverageConfidence prop
  | and [ sufficientlyCovered coverageConfidence tot n p
        | (_, _, tot, n, p) <- allCoverage classes tables requiredCoverage numTests
        ] = once prop
  | or [ insufficientlyCovered (Just (certainty coverageConfidence)) tot n p
       | (_, _, tot, n, p) <- allCoverage classes tables requiredCoverage numTests
       ] = let (theLabels, theTables) = labelsAndTables labels classes tables requiredCoverage numTests (Just coverageConfidence) in
           foldr counterexample (property failed{P.reason = "Insufficient coverage"})
             (paragraphs [theLabels, theTables])
  | otherwise = prop

allCoverage :: Map.Map String Int -> Map.Map String (Map.Map String Int) -> Map.Map (Maybe String, String) Double -> Int -> [(Maybe String, String, Int, Int, Double)]
allCoverage classes tables requiredCoverage numTests =
  [ (key, value, tot, n, p)
  | ((key, value), p) <- Map.toList requiredCoverage,
    let tot = case key of
                Just key -> Map.findWithDefault 0 key totals
                Nothing -> numTests,
    let n = Map.findWithDefault 0 value (Map.findWithDefault Map.empty key combinedCounts)
  ]
  where
    combinedCounts :: Map.Map (Maybe String) (Map.Map String Int)
    combinedCounts = Map.insert Nothing classes (Map.mapKeys Just tables)

    totals :: Map.Map String Int
    totals = fmap (sum . Map.elems) tables

--------------------------------------------------------------------------
-- the end.
