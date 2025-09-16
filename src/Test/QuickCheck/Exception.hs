-- | Throwing and catching exceptions. Internal QuickCheck module.

-- Hide away the nasty implementation-specific ways of catching
-- exceptions behind a nice API. The main trouble is catching ctrl-C.

{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE CPP #-}
#ifndef NO_SAFE_HASKELL
{-# LANGUAGE Safe #-}
#endif
module Test.QuickCheck.Exception where

#if !defined(__GLASGOW_HASKELL__) || (__GLASGOW_HASKELL__ < 700)
#define OLD_EXCEPTIONS
#endif

#if defined(NO_EXCEPTIONS)
#else
import qualified Control.Exception as E
#endif

#if defined(NO_EXCEPTIONS)
type AnException = ()
#elif defined(OLD_EXCEPTIONS)
type AnException = E.Exception
#else
type AnException = E.SomeException
#endif

#ifdef NO_EXCEPTIONS
tryEvaluate :: a -> IO (Either AnException a)
tryEvaluate x = return (Right x)

tryEvaluateIO :: IO a -> IO (Either AnException a)
tryEvaluateIO m = fmap Right m

evaluate :: a -> IO a
evaluate x = x `seq` return x

isInterrupt :: AnException -> Bool
isInterrupt _ = False

discard :: a
discard = error "'discard' not supported, since your Haskell system can't catch exceptions"

isDiscard :: AnException -> Bool
isDiscard _ = False

finally :: IO a -> IO b -> IO a
finally mx my = do
  x <- mx
  my
  return x

#else
--------------------------------------------------------------------------
-- try evaluate

tryEvaluate :: a -> IO (Either AnException a)
tryEvaluate x = tryEvaluateIO (return x)

tryEvaluateIO :: IO a -> IO (Either AnException a)
tryEvaluateIO m = E.tryJust notAsync (m >>= E.evaluate)
  where
    notAsync :: AnException -> Maybe AnException
#if MIN_VERSION_base(4,7,0)
    notAsync e = case E.fromException e of
        Just (E.SomeAsyncException _) -> Nothing
        Nothing                       -> Just e
#elif !defined(OLD_EXCEPTIONS)
    notAsync e = case E.fromException e :: Maybe E.AsyncException of
        Just _  -> Nothing
        Nothing -> Just e
#else
    notAsync e = Just e
#endif

evaluate :: a -> IO a
evaluate = E.evaluate

-- | Test if an exception was a @^C@.
-- QuickCheck won't try to shrink an interrupted test case.
isInterrupt :: AnException -> Bool

#if defined(OLD_EXCEPTIONS)
isInterrupt _ = False
#else
isInterrupt e = E.fromException e == Just E.UserInterrupt
#endif

-- | A special error value. If a property evaluates 'discard', it
-- causes QuickCheck to discard the current test case.
-- This can be useful if you want to discard the current test case,
-- but are somewhere you can't use 'Test.QuickCheck.==>', such as inside a
-- generator.
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
  isDiscard e =
    case E.fromException e of
      Just (E.ErrorCall msg') -> msg' == msg
      _ -> False
#endif

finally :: IO a -> IO b -> IO a
finally = E.finally
#endif

--------------------------------------------------------------------------
-- the end.
