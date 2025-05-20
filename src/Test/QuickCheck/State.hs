{-# LANGUAGE CPP #-}
#ifndef NO_SAFE_HASKELL
{-# LANGUAGE Safe #-}
#endif
{-# OPTIONS_HADDOCK hide #-}
-- | QuickCheck's internal state. Internal QuickCheck module.
module Test.QuickCheck.State where

import Test.QuickCheck.Text
import Test.QuickCheck.Random
import Data.Map(Map)

--------------------------------------------------------------------------
-- State

-- | State represents QuickCheck's internal state while testing a property.
-- The state is made visible to callback functions.
data State
  = MkState
  -- static
  { terminal                  :: Terminal
    -- ^ the current terminal
  , maxSuccessTests           :: Int
    -- ^ maximum number of successful tests needed
  , maxDiscardedRatio         :: Int
    -- ^ maximum number of discarded tests per successful test
  , coverageConfidence        :: Maybe Confidence
    -- ^ how to compute the size of test cases from
    --   #tests and #discarded tests
  , numTotMaxShrinks          :: !Int
    -- ^ How many shrinks to try before giving up
  , replayStartSize           :: Maybe Int
    -- ^ Size to start at when replaying
  , maxTestSize               :: !Int
    -- ^ Maximum size of test

    -- dynamic
  , numSuccessTests           :: !Int
    -- ^ the current number of tests that have succeeded
  , numDiscardedTests         :: !Int
    -- ^ the current number of discarded tests
  , numRecentlyDiscardedTests :: !Int
    -- ^ the number of discarded tests since the last successful test
  , labels                    :: !(Map [String] Int)
    -- ^ counts for each combination of labels (label/collect)
  , classes                   :: !(Map String Int)
    -- ^ counts for each class of test case (classify/cover)
  , tables                    :: !(Map String (Map String Int))
    -- ^ tables collected using tabulate
  , requiredCoverage          :: !(Map (Maybe String, String) Double)
    -- ^ coverage requirements
  , expected                  :: !Bool
    -- ^ indicates the expected result of the property
  , randomSeed                :: !QCGen
    -- ^ the current random seed

    -- shrinking
  , numSuccessShrinks         :: !Int
    -- ^ number of successful shrinking steps so far
  , numTryShrinks             :: !Int
    -- ^ number of failed shrinking steps since the last successful shrink
  , numTotTryShrinks          :: !Int
    -- ^ total number of failed shrinking steps
  }

-- | The statistical parameters used by 'Test.QuickCheck.checkCoverage'.
data Confidence =
  Confidence {
    certainty :: Integer,
    -- ^ How certain 'Test.QuickCheck.checkCoverage' must be before the property
    -- fails. If the coverage requirement is met, and the certainty parameter is
    -- @n@, then you should get a false positive at most one in @n@ runs of
    -- QuickCheck. The default value is @10^9@.
    --
    -- Lower values will speed up 'Test.QuickCheck.checkCoverage' at the cost of
    -- false positives.
    --
    -- If you are using 'Test.QuickCheck.checkCoverage' as part of a test suite,
    -- you should be careful not to set @certainty@ too low. If you want, say, a
    -- 1% chance of a false positive during a project's lifetime, then
    -- certainty@ should be set to at least @100 * m * n@, where @m@ is the
    -- number of uses of 'Test.QuickCheck.cover' in the test suite, and @n@ is
    -- the number of times you expect the test suite to be run during the
    -- project's lifetime. The default value is chosen to be big enough for most
    -- projects.
    tolerance :: Double
    -- ^ For statistical reasons, 'Test.QuickCheck.checkCoverage' will not
    -- reject coverage levels that are only slightly below the required levels.
    -- If the required level is @p@ then an actual level of @tolerance * p@
    -- will be accepted. The default value is @0.9@.
    --
    -- Lower values will speed up 'Test.QuickCheck.checkCoverage' at the cost of
    -- not detecting minor coverage violations.
    }
  deriving Show

-- | TestProgress, contains information that might be interesting to external
-- libraries, e.g. Tasty. From this it is possible to install your own callbacks
-- that reports e.g. progress.
data TestProgress
  = TestProgress
  {
    currentPassed        :: Int -- ^ Number of tests passed so far
  , currentDiscarded     :: Int -- ^ Number of discared tests so far
  , maxTests             :: Int -- ^ Number of tests to execute

  , currentShrinks       :: Int -- ^ Number of successful shrinking steps
  , currentFailedShrinks :: Int -- ^ Number of failed shrinking steps since last successful one
  , currentTotalShrinks  :: Int -- ^ Total number of failed shrinking steps
  } deriving Show

--------------------------------------------------------------------------
-- the end.
