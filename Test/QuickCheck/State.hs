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

data Confidence =
  Confidence {
    certainty :: Integer,
    tolerance :: Double }
  deriving Show

--------------------------------------------------------------------------
-- the end.
