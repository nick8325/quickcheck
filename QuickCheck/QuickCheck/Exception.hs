module Chalmers.QuickCheck.Exception where

import Control.Exception
  ( evaluate
  , try
  , Exception
  )

--------------------------------------------------------------------------
-- try evaluate

tryEvaluate :: a -> IO (Either Exception a)
tryEvaluate x = tryEvaluateIO (return x)

tryEvaluateIO :: IO a -> IO (Either Exception a)
tryEvaluateIO m = try (m >>= evaluate)
--tryEvaluateIO m = Right `fmap` m

--------------------------------------------------------------------------
-- the end.
