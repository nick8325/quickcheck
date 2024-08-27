{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}

module Test.QuickCheck.Monoids
  ( Every (..)
  , Some (..)
  ) where

#ifndef NO_SEMIGROUP
import Data.List.NonEmpty as NonEmpty
import Data.Semigroup (Semigroup (..))
#else
import Data.Monoid (Monoid (..))
#endif
import Test.QuickCheck.Property

-- | Conjunction monoid built with `.&&.`.
--
-- Use `property @Every` as an accessor which doesn't leak
-- existential variables.
--
-- Note: monoid laws are satisfied up to `isSuccess` unless one is using
-- `checkCoverage`.
--
#ifndef NO_EXISTENTIAL_FIELD_SELECTORS
data Every = forall p. Testable p => Every { getEvery :: p }
#else
data Every = forall p. Testable p => Every p
#endif

instance Testable Every where
    property (Every p) = property p

#ifndef NO_SEMIGROUP
instance Semigroup Every where
    Every p <> Every p' = Every (p .&&. p')
    sconcat = Every . conjoin . NonEmpty.toList

instance Monoid Every where
    mempty = Every True
    mappend = (<>)
    mconcat = Every . conjoin
#else
instance Monoid Every where
    mempty = Every True
    mappend (Every p) (Every p') = Every (p .&&. p')
    mconcat = Every . conjoin
#endif


-- | Disjunction monoid built with `.||.`.
--
-- Use `property @Some` as an accessor which doesn't leak
-- existential variables.
--
-- Note: monoid laws are satisfied up to `isSuccess` unless one is using
-- `checkCoverage`.
--
#ifndef NO_EXISTENTIAL_FIELD_SELECTORS
data Some = forall p. Testable p => Some { getSome :: p }
#else
data Some = forall p. Testable p => Some p
#endif

instance Testable Some where
    property (Some p) = property p

#ifndef NO_SEMIGROUP
instance Semigroup Some where
    Some p <> Some p' = Some (p .||. p')
    sconcat = Some . disjoin . NonEmpty.toList

instance Monoid Some where
    mempty = Some False
    mappend = (<>)
    mconcat = Some . disjoin
#else
instance Monoid Some where
    mempty = Some False
    mappend (Some p) (Some p') = Some (p .||. p')
    mconcat = Some . disjoin
#endif
