{-# LANGUAGE CPP #-}
module Test.QuickCheck.Exception where

#if !defined(__GLASGOW_HASKELL__) || (__GLASGOW_HASKELL__ >= 609)
#define EXCEPTION SomeException
#else
#define EXCEPTION Exception
#endif

import Control.Exception
  ( evaluate
  , try
  , EXCEPTION
  )

--------------------------------------------------------------------------
-- try evaluate

tryEvaluate :: a -> IO (Either EXCEPTION a)
tryEvaluate x = tryEvaluateIO (return x)

tryEvaluateIO :: IO a -> IO (Either EXCEPTION a)
tryEvaluateIO m = try (m >>= evaluate)
--tryEvaluateIO m = Right `fmap` m

--------------------------------------------------------------------------
-- the end.
