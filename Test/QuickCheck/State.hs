module Test.QuickCheck.State where

import Test.QuickCheck.Text
import System.Random( StdGen )

--------------------------------------------------------------------------
-- State

data State
  = MkState
  -- static
  { terminal          :: Terminal
  , maxSuccessTests   :: Int
  , maxTryTests       :: Int
  , maxSize           :: Int
  
  -- dynamic
  , numSuccessTests   :: Int
  , numTryTests       :: Int
  , collected         :: [[(String,Int)]]
  , expectedFailure   :: Bool
  , randomSeed        :: StdGen
  
  -- shrinking
  , isShrinking       :: Bool
  , numSuccessShrinks :: Int
  , numTryShrinks     :: Int
  }

--------------------------------------------------------------------------
-- the end.
