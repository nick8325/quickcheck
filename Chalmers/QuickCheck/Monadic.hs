{-# OPTIONS_GHC -fglasgow-exts #-}
module Chalmers.QuickCheck.Monadic where

--------------------------------------------------------------------------
-- imports

import Chalmers.QuickCheck.Gen
import Chalmers.QuickCheck.Property
import Chalmers.QuickCheck.Arbitrary

import Control.Monad
  ( liftM
  )

import Control.Monad.ST

import System.IO.Unsafe
  ( unsafePerformIO
  )

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
  fail s              = MkPropertyM (\k -> return (return (property result)))
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
monadic run (MkPropertyM m) =
  do mp <- m (const (return (return (property True))))
     run mp

{-
monadicIO :: Monad m => (m Property -> IO Property) -> PropertyM m a -> IO Property
monadicIO run (MkPropertyM m) =
  do mp <- m (const (return (return (property True))))
     run mp
-}

-- Can't make this work in any other way... :-(
monadicIO :: PropertyM IO a -> Property
monadicIO (MkPropertyM m) =
  property $
    unsafePerformIO `fmap`
      m (const (return (return (property True))))

newtype IdM m s a = MkIdM { unIdM :: m s a }

data MonadS' m
  = MkMonadS
  { ret :: forall a   s . a -> m s a
  , bin :: forall a b s . m s a -> (a -> m s b) -> m s b
  }

--grab () = MkMonadS return (>>=)

class MonadS m where
  return' :: a -> m s a
  bind'   :: m s a -> (a -> m s b) -> m s b

instance MonadS m => Monad (IdM m s) where
  return = MkIdM . return'
  MkIdM m >>= k = MkIdM (m `bind'` (unIdM . k))

{-
monadicS :: MonadS m => ((forall s . m s Property) -> Property) -> (forall s . PropertyM (m s) a) -> Property
monadicS run mp = MkGen $ \r n ->
  let MkGen g'      = run (let MkPropertyM f = mp'                                        
                               MkGen g       = f (const (return (return (property True))))
                            in unIdM (g r n))
   in g' undefined undefined
 where
  mp' = MkPropertyM (\k -> fmap MkIdM (unPropertyM mp (\a -> fmap unIdM (k a))))
-}

{-

-- does not compile with GHC 6.6
imperative :: (forall s. PropertyM (ST s) a) -> Property
imperative m = MkGen $ \r n ->
  let MkPropertyM f = m
      MkGen g = f (const (return (return (property True))))
      MkGen q = runST (g r n)
   in q undefined undefined
-}

--------------------------------------------------------------------------
-- the end.
