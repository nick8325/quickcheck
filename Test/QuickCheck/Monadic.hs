{-# LANGUAGE CPP #-}
#ifndef NO_SAFE_HASKELL
#if !defined(NO_ST_MONAD) && !(MIN_VERSION_base(4,8,0))
{-# LANGUAGE Trustworthy #-}
#else
{-# LANGUAGE Safe #-}
#endif
#endif
#ifndef NO_ST_MONAD
{-# LANGUAGE Rank2Types #-}
#endif
{-|
Module   : Test.QuickCheck.Monadic

Allows testing of monadic values. Will generally follow this form:

@
prop_monadic a b = 'monadicIO' $ do
  a\' \<- 'run' (f a)
  b\' \<- 'run' (f b)
  -- ...
  'assert' someBoolean
@

Example using the @FACTOR(1)@ command-line utility:

@
import System.Process
import Test.QuickCheck
import Test.QuickCheck.Monadic

-- $ factor 16
-- 16: 2 2 2 2
factor :: Integer -> IO [Integer]
factor n = parse \`fmap\` 'System.Process.readProcess' \"factor\" [show n] \"\" where

  parse :: String -> [Integer]
  parse = map read . tail . words

prop_factor :: Positive Integer -> Property
prop_factor ('Test.QuickCheck.Modifiers.Positive' n) = 'monadicIO' $ do
  factors \<- 'run' (factor n)

  'assert' (product factors == n)
@

>>> quickCheck prop_factor
+++ OK, passed 100 tests.

See the paper \"<http://www.cse.chalmers.se/~rjmh/Papers/QuickCheckST.ps Testing Monadic Code with QuickCheck>\".
-}
module Test.QuickCheck.Monadic (
  -- * Property monad
    PropertyM(..)

  -- * Monadic specification combinators
  , run
  , assert
  , assertWith
  , pre
  , wp
  , pick
  , forAllM
  , monitor
  , stop

  -- * Run functions
  , monadic
  , monadic'
  , monadicIO
#ifndef NO_ST_MONAD
  , monadicST
  , runSTGen
#endif
  ) where

--------------------------------------------------------------------------
-- imports

import Test.QuickCheck.Gen
import Test.QuickCheck.Gen.Unsafe
import Test.QuickCheck.Property

import Control.Monad(liftM, liftM2)

import Control.Monad.ST
import Control.Applicative

#ifndef NO_TRANSFORMERS
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
#endif

#ifndef NO_MONADFAIL
import qualified Control.Monad.Fail as Fail
#endif

--------------------------------------------------------------------------
-- type PropertyM

-- | The property monad is really a monad transformer that can contain
-- monadic computations in the monad @m@ it is parameterized by:
--
--   * @m@ - the @m@-computations that may be performed within @PropertyM@
--
-- Elements of @PropertyM m a@ may mix property operations and @m@-computations.
newtype PropertyM m a =
  MkPropertyM { unPropertyM :: (a -> Gen (m Property)) -> Gen (m Property) }

bind :: PropertyM m a -> (a -> PropertyM m b) -> PropertyM m b
MkPropertyM m `bind` f = MkPropertyM (\k -> m (\a -> unPropertyM (f a) k))

fail_ :: Monad m => String -> PropertyM m a
fail_ s = stop (failed { reason = s })

instance Functor (PropertyM m) where
  fmap f (MkPropertyM m) = MkPropertyM (\k -> m (k . f))

instance Applicative (PropertyM m) where
  pure x = MkPropertyM (\k -> k x)
  mf <*> mx =
    mf `bind` \f -> mx `bind` \x -> pure (f x)

instance Monad m => Monad (PropertyM m) where
  return = pure
  (>>=) = bind
#if !MIN_VERSION_base(4,13,0)
  fail = fail_
#endif

#ifndef NO_MONADFAIL
instance Monad m => Fail.MonadFail (PropertyM m) where
  fail = fail_
#endif

#ifndef NO_TRANSFORMERS
instance MonadTrans PropertyM where
  lift = run

instance MonadIO m => MonadIO (PropertyM m) where
  liftIO = run . liftIO
#endif

stop :: (Testable prop, Monad m) => prop -> PropertyM m a
stop p = MkPropertyM (\_k -> return (return (property p)))

-- should think about strictness/exceptions here
-- assert :: Testable prop => prop -> PropertyM m ()
-- | Allows embedding non-monadic properties into monadic ones.
assert :: Monad m => Bool -> PropertyM m ()
assert True  = return ()
assert False = fail "Assertion failed"

-- | Like 'assert' but allows caller to specify an explicit message to show on failure.
--
-- __Example:__
--
-- @
-- do
--   assertWith True  "My first predicate."
--   assertWith False "My other predicate."
--   ...
-- @
--
-- @
-- Assertion failed (after 2 tests):
--     Passed: My first predicate
--     Failed: My other predicate
-- @
assertWith :: Monad m => Bool -> String -> PropertyM m ()
assertWith condition msg = do
    let prefix = if condition then "Passed: " else "Failed: "
    monitor $ counterexample $ prefix <> msg
    assert condition

-- should think about strictness/exceptions here
-- | Tests preconditions. Unlike 'assert' this does not cause the
-- property to fail, rather it discards them just like using the
-- implication combinator 'Test.QuickCheck.Property.==>'.
--
-- This allows representing the <https://en.wikipedia.org/wiki/Hoare_logic Hoare triple>
--
-- > {p} x ← e{q}
--
-- as
--
-- @
-- pre p
-- x \<- run e
-- assert q
-- @
--
pre :: Monad m => Bool -> PropertyM m ()
pre True  = return ()
pre False = stop rejected

-- should be called lift?
-- | The lifting operation of the property monad. Allows embedding
-- monadic\/'IO'-actions in properties:
--
-- @
-- log :: Int -> IO ()
--
-- prop_foo n = monadicIO $ do
--   run (log n)
--   -- ...
-- @
run :: Monad m => m a -> PropertyM m a
run m = MkPropertyM (liftM (m >>=) . promote)

-- | Quantification in a monadic property, fits better with
-- /do-notation/ than 'forAllM'.
-- __Note__: values generated by 'pick' do not shrink.
pick :: (Monad m, Show a) => Gen a -> PropertyM m a
pick gen = MkPropertyM $ \k ->
  do a <- gen
     mp <- k a
     return (do p <- mp
                return (forAll (return a) (const p)))

-- | The <https://en.wikipedia.org/wiki/Predicate_transformer_semantics#Weakest_preconditions weakest precondition>
--
-- > wp(x ← e, p)
--
-- can be expressed as in code as @wp e (\\x -> p)@.
wp :: Monad m => m a -> (a -> PropertyM m b) -> PropertyM m b
wp m k = run m >>= k

-- | Quantification in monadic properties to 'pick', with a notation similar to
-- 'forAll'. __Note__: values generated by 'forAllM' do not shrink.

forAllM :: (Monad m, Show a) => Gen a -> (a -> PropertyM m b) -> PropertyM m b
forAllM gen k = pick gen >>= k

-- | Allows making observations about the test data:
--
-- @
-- monitor ('collect' e)
-- @
--
-- collects the distribution of value of @e@.
--
-- @
-- monitor ('counterexample' "Failure!")
-- @
--
-- Adds @"Failure!"@ to the counterexamples.
monitor :: Monad m => (Property -> Property) -> PropertyM m ()
monitor f = MkPropertyM (\k -> (f `liftM`) `fmap` (k ()))

-- run functions

monadic :: (Testable a, Monad m) => (m Property -> Property) -> PropertyM m a -> Property
monadic runner m = property (fmap runner (monadic' m))

monadic' :: (Testable a, Monad m) => PropertyM m a -> Gen (m Property)
monadic' (MkPropertyM m) = m (\prop -> return (return (property prop)))

-- | Runs the property monad for 'IO'-computations.
--
-- @
-- prop_cat msg = monadicIO $ do
--   (exitCode, stdout, _) \<- run ('System.Process.readProcessWithExitCode' "cat" [] msg)
--
--   pre ('System.Exit.ExitSuccess' == exitCode)
--
--   assert (stdout == msg)
-- @
--
-- >>> quickCheck prop_cat
-- +++ OK, passed 100 tests.
--
monadicIO :: Testable a => PropertyM IO a -> Property
monadicIO = monadic ioProperty

#ifndef NO_ST_MONAD
-- | Runs the property monad for 'ST'-computations.
--
-- @
-- -- Your mutable sorting algorithm here
-- sortST :: Ord a => [a] -> 'Control.Monad.ST.ST' s (MVector s a)
-- sortST = 'Data.Vector.thaw' . 'Data.Vector.fromList' . 'Data.List.sort'
--
-- prop_sortST xs = monadicST $ do
--   sorted  \<- run ('Data.Vector.freeze' =<< sortST xs)
--   assert ('Data.Vector.toList' sorted == sort xs)
-- @
--
-- >>> quickCheck prop_sortST
-- +++ OK, passed 100 tests.
--
monadicST :: Testable a => (forall s. PropertyM (ST s) a) -> Property
monadicST m = property (runSTGen (monadic' m))

runSTGen :: (forall s. Gen (ST s a)) -> Gen a
runSTGen f = do
  Capture eval <- capture
  return (runST (eval f))
#endif

--------------------------------------------------------------------------
-- the end.
