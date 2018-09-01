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
    -- ^ required coverage confidence
  , computeSize               :: Int -> Int -> Int
    -- ^ how to compute the size of test cases from
    --   #tests and #discarded tests
  , numTotMaxShrinks          :: !Int
    -- ^ How many shrinks to try before giving up

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

-- | The statistical parameters used by 'checkCoverage'.
data Confidence =
  Confidence {
    certainty :: Integer,
    -- ^ How certain 'checkCoverage' must be before the property fails.
    -- If the coverage requirement is met, and the certainty parameter is @n@,
    -- then you should get a false positive at most one in @n@ runs of QuickCheck.
    -- The default value is @10^9@.
    tolerance :: Double
    -- ^ For statistical reasons, 'checkCoverage' will not reject coverage
    -- levels that are only slightly below the required levels.
    -- If the required level is @p@ then an actual level of @tolerance * p@
    -- will be accepted. The default value is @0.9@.
    }
  deriving Show

--------------------------------------------------------------------------
-- the end.
