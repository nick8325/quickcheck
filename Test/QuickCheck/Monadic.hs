{-# LANGUAGE CPP #-}
#ifndef NO_ST_MONAD
{-# LANGUAGE Rank2Types #-}
#endif
-- | Allows testing of monadic values.
-- See the paper \"Testing Monadic Code with QuickCheck\":
-- <http://www.cse.chalmers.se/~rjmh/Papers/QuickCheckST.ps>.
module Test.QuickCheck.Monadic where

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

--------------------------------------------------------------------------
-- type PropertyM

newtype PropertyM m a =
  MkPropertyM { unPropertyM :: (a -> Gen (m Property)) -> Gen (m Property) }

instance Functor (PropertyM m) where
  fmap f (MkPropertyM m) = MkPropertyM (\k -> m (k . f))

instance Monad m => Applicative (PropertyM m) where
  pure = return
  (<*>) = liftM2 ($)

instance Monad m => Monad (PropertyM m) where
  return x            = MkPropertyM (\k -> k x)
  MkPropertyM m >>= f = MkPropertyM (\k -> m (\a -> unPropertyM (f a) k))
  fail s              = stop (failed { reason = s })

#ifndef NO_TRANSFORMERS
instance MonadTrans PropertyM where
  lift = run

instance MonadIO m => MonadIO (PropertyM m) where
  liftIO = run . liftIO
#endif

stop :: (Testable prop, Monad m) => prop -> PropertyM m a
stop p = MkPropertyM (\_k -> return (return (property p)))

-- should think about strictness/exceptions here
--assert :: Testable prop => prop -> PropertyM m ()
assert :: Monad m => Bool -> PropertyM m ()
assert True = return ()
assert False = fail "Assertion failed"

-- should think about strictness/exceptions here
pre :: Monad m => Bool -> PropertyM m ()
pre True = return ()
pre False = stop rejected

-- should be called lift?
run :: Monad m => m a -> PropertyM m a
run m = MkPropertyM (liftM (m >>=) . promote)

pick :: (Monad m, Show a) => Gen a -> PropertyM m a
pick gen = MkPropertyM $ \k ->
  do a <- gen
     mp <- k a
     return (do p <- mp
                return (forAll (return a) (const p)))

wp :: Monad m => m a -> (a -> PropertyM m b) -> PropertyM m b
wp m k = run m >>= k

forAllM :: (Monad m, Show a) => Gen a -> (a -> PropertyM m b) -> PropertyM m b
forAllM gen k = pick gen >>= k

monitor :: Monad m => (Property -> Property) -> PropertyM m ()
monitor f = MkPropertyM (\k -> (f `liftM`) `fmap` (k ()))

-- run functions

monadic :: Monad m => (m Property -> Property) -> PropertyM m a -> Property
monadic runner m = property (fmap runner (monadic' m))

monadic' :: Monad m => PropertyM m a -> Gen (m Property)
monadic' (MkPropertyM m) = m (const (return (return (property True))))

monadicIO :: PropertyM IO a -> Property
monadicIO = monadic ioProperty

#ifndef NO_ST_MONAD
monadicST :: (forall s. PropertyM (ST s) a) -> Property
monadicST m = property (runSTGen (monadic' m))

runSTGen :: (forall s. Gen (ST s a)) -> Gen a
runSTGen f = do
  Capture eval <- capture
  return (runST (eval f))
#endif

--------------------------------------------------------------------------
-- the end.
