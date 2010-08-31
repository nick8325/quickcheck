{-# LANGUAGE Rank2Types #-}
-- | Allows testing of monadic values.
module Test.QuickCheck.Monadic where

--------------------------------------------------------------------------
-- imports

import Test.QuickCheck.Gen
import Test.QuickCheck.Property
import Test.QuickCheck.Arbitrary

import Control.Monad
  ( liftM
  )

import Control.Monad.ST

-- instance of monad transformer?

--------------------------------------------------------------------------
-- type PropertyM

newtype PropertyM m a =
  MkPropertyM { unPropertyM :: (a -> Gen (m Property)) -> Gen (m Property) }

instance Functor (PropertyM m) where
  fmap f (MkPropertyM m) = MkPropertyM (\k -> m (k . f))

instance Monad m => Monad (PropertyM m) where
  return x            = MkPropertyM (\k -> k x)
  MkPropertyM m >>= f = MkPropertyM (\k -> m (\a -> unPropertyM (f a) k))
  fail s              = MkPropertyM (\_ -> return (return (property result)))
   where
    result = failed{ reason = s }

-- should think about strictness/exceptions here
--assert :: Testable prop => prop -> PropertyM m ()
assert :: Monad m => Bool -> PropertyM m ()
assert b = MkPropertyM $ \k ->
  if b
    then k ()
    else return (return (property False))

{-
let Prop p = property a in Monadic $ \k ->
  do r <- p
     case ok r of
       Just True -> do m <- k ()
                       return (do p' <- m
		                  return (r &&& p'))
       _ -> return (return (property r))
-}

-- should think about strictness/exceptions here
pre :: Monad m => Bool -> PropertyM m ()
pre b = MkPropertyM $ \k ->
  if b
    then k ()
    else return (return (property ()))

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
monadicIO = monadic property

imperative :: (forall s. PropertyM (ST s) a) -> Property
imperative m = property (runSTGen (monadic' m))

runSTGen :: (forall s. Gen (ST s a)) -> Gen a
runSTGen g = MkGen $ \r n -> runST (unGen g r n)

--------------------------------------------------------------------------
-- the end.
