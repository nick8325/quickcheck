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

#if __GLASGOW_HASKELL__ >= 700
#define NO_BASE_3
#endif
#endif

#if defined(OLD_EXCEPTIONS) || defined(NO_BASE_3)
import qualified Control.Exception as E
#else
import qualified Control.Exception.Extensible as E
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
type AnException = E.Exception
#else
type AnException = E.SomeException
#endif

--------------------------------------------------------------------------
-- try evaluate

tryEvaluate :: a -> IO (Either AnException a)
tryEvaluate x = tryEvaluateIO (return x)

tryEvaluateIO :: IO a -> IO (Either AnException a)
tryEvaluateIO m = E.try (m >>= E.evaluate)
--tryEvaluateIO m = Right `fmap` m

-- Test if an exception was a ^C.
-- QuickCheck won't try to shrink an interrupted test case.
isInterrupt :: AnException -> Bool

#if defined(GHC_INTERRUPT)
#if defined(OLD_EXCEPTIONS)
isInterrupt (E.DynException e) = fromDynamic e == Just Interrupted
isInterrupt _ = False
#elif defined(GHCI_INTERRUPTED_EXCEPTION)
isInterrupt (E.SomeException e) =
  cast e == Just Interrupted || cast e == Just E.UserInterrupt
#else
isInterrupt e = E.fromException e == Just E.UserInterrupt
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
(discard, isDiscard) = (E.throw (E.ErrorCall msg), isDiscard)
 where
  msg = "DISCARD. " ++
        "You should not see this exception, it is internal to QuickCheck."
#if defined(OLD_EXCEPTIONS)
  isDiscard (E.ErrorCall msg') = msg' == msg
  isDiscard _ = False
#else
  isDiscard (E.SomeException e) =
    case cast e of
      Just (E.ErrorCall msg') -> msg' == msg
      _ -> False
#endif

finally :: IO a -> IO b -> IO a
finally = E.finally

--------------------------------------------------------------------------
-- the end.
