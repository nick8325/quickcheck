{-# LANGUAGE CPP #-}
module Test.QuickCheck.Exception where

#if defined(MIN_VERSION_base)
#if !(MIN_VERSION_base(4,0,0))
#define SomeException Exception
#endif
#endif

#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ < 609)
#define SomeException Exception
#endif

import Control.Exception
  ( evaluate
  , try
  , SomeException
  )

--------------------------------------------------------------------------
-- try evaluate

tryEvaluate :: a -> IO (Either SomeException a)
tryEvaluate x = tryEvaluateIO (return x)

tryEvaluateIO :: IO a -> IO (Either SomeException a)
tryEvaluateIO m = try (m >>= evaluate)
--tryEvaluateIO m = Right `fmap` m

--------------------------------------------------------------------------
-- the end.
