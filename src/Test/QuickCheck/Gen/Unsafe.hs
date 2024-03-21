{-# LANGUAGE CPP #-}
#ifndef NO_SAFE_HASKELL
{-# LANGUAGE Safe #-}
#endif
#ifndef NO_ST_MONAD
{-# LANGUAGE Rank2Types #-}
#endif
-- | Unsafe combinators for the 'Gen' monad.
--
-- 'Gen' is only morally a monad: two generators that are supposed
-- to be equal will give the same probability distribution, but they
-- might be different as functions from random number seeds to values.
-- QuickCheck maintains the illusion that a 'Gen' is a probability
-- distribution and does not allow you to distinguish two generators
-- that have the same distribution.
--
-- The functions in this module allow you to break this illusion by
-- reusing the same random number seed twice. This is unsafe because
-- by applying the same seed to two morally equal generators, you can
-- see whether they are really equal or not.
module Test.QuickCheck.Gen.Unsafe where

import Test.QuickCheck.Gen
import Control.Monad
import Control.Applicative

-- | Promotes a monadic generator to a generator of monadic values.
promote :: Monad m => m (Gen a) -> Gen (m a)
promote m = flip liftM m <$> delay

-- | Randomly generates a function of type @'Gen' a -> a@, which
-- you can then use to evaluate generators. Mostly useful in
-- implementing 'promote'.
delay :: Gen (Gen a -> a)
delay = MkGen (\r n g -> unGen g r n)

#ifndef NO_ST_MONAD
-- | A variant of 'delay' that returns a polymorphic evaluation function.
-- Can be used in a pinch to generate polymorphic (rank-2) values:
--
-- > genSelector :: Gen (a -> a -> a)
-- > genSelector = elements [\x y -> x, \x y -> y]
-- >
-- > data Selector = Selector (forall a. a -> a -> a)
-- > genPolySelector :: Gen Selector
-- > genPolySelector = do
-- >   Capture eval <- capture
-- >   return (Selector (eval genSelector))
capture :: Gen Capture
capture = MkGen (\r n -> Capture (\g -> unGen g r n))

newtype Capture = Capture (forall a. Gen a -> a)
#endif
