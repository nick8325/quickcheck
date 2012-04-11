-- Hide away the nasty implementation-specific ways of catching
-- exceptions behind a nice API. The main trouble is catching ctrl-C.

{-# LANGUAGE CPP #-}
module Test.QuickCheck.Exception where

#if !defined(__GLASGOW_HASKELL__) || (__GLASGOW_HASKELL__ < 609)
#define OLD_EXCEPTIONS
#endif

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 607
#define GHC_INTERRUPT

#if __GLASGOW_HASKELL__ < 613
#define GHCI_INTERRUPTED_EXCEPTION
#endif
#endif

#if defined(OLD_EXCEPTIONS)
import Control.Exception(evaluate, try, Exception(..), throw)
#else
import Control.Exception.Extensible(evaluate, try, SomeException(SomeException), ErrorCall(..), throw
#if defined(GHC_INTERRUPT)
  , AsyncException(UserInterrupt)
#endif
  )
#endif

#if defined(GHC_INTERRUPT)
#if defined(GHCI_INTERRUPTED_EXCEPTION)
import Panic(GhcException(Interrupted))
#endif
import Data.Typeable
#if defined(OLD_EXCEPTIONS)
import Data.Dynamic
#endif
#endif

#if defined(OLD_EXCEPTIONS)
type AnException = Control.Exception.Exception
#else
type AnException = SomeException
#endif

--------------------------------------------------------------------------
-- try evaluate

tryEvaluate :: a -> IO (Either AnException a)
tryEvaluate x = tryEvaluateIO (return x)

tryEvaluateIO :: IO a -> IO (Either AnException a)
tryEvaluateIO m = try (m >>= evaluate)
--tryEvaluateIO m = Right `fmap` m

-- Test if an exception was a ^C.
-- QuickCheck won't try to shrink an interrupted test case.
isInterrupt :: AnException -> Bool

#if defined(GHC_INTERRUPT)
#if defined(OLD_EXCEPTIONS)
isInterrupt (DynException e) = fromDynamic e == Just Interrupted
isInterrupt _ = False
#elif defined(GHCI_INTERRUPTED_EXCEPTION)
isInterrupt (SomeException e) =
  cast e == Just Interrupted || cast e == Just UserInterrupt
#else
isInterrupt (SomeException e) = cast e == Just UserInterrupt
#endif

#else /* !defined(GHC_INTERRUPT) */
isInterrupt _ = False
#endif

-- | A special exception that makes QuickCheck discard the test case.
-- Normally you should use '==>', but if for some reason this isn't
-- possible (e.g. you are deep inside a generator), use 'discard'
-- instead.
discard :: a

isDiscard :: AnException -> Bool
(discard, isDiscard) = (throw (ErrorCall msg), isDiscard)
 where
  msg = "DISCARD. " ++
        "You should not see this exception, it is internal to QuickCheck."
#if defined(OLD_EXCEPTIONS)
  isDiscard (ErrorCall msg') = msg' == msg
  isDiscard _ = False
#else
  isDiscard (SomeException e) =
    case cast e of
      Just (ErrorCall msg') -> msg' == msg
      _ -> False
#endif


--------------------------------------------------------------------------
-- the end.
