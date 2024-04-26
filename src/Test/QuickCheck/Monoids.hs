{-# LANGUAGE ExistentialQuantification #-}

module Test.QuickCheck.Monoids
  ( Every (..)
  , Some (..)
  ) where

import Data.List.NonEmpty as NonEmpty
import Data.Semigroup (Semigroup (..))
import Test.QuickCheck.Property

-- | Conjunction monoid built with `.&&.`.
--
-- Use `property @Every` as an accessor which doesn't leak
-- existential variables.
--
-- Note: monoid laws are satisfied up to `isSuccess` unless one is using
-- `checkCoverage`.
--
data Every = forall p. Testable p => Every { getEvery :: p }

instance Testable Every where
    property (Every p) = property p

instance Semigroup Every where
    Every p <> Every p' = Every (p .&&. p')
    sconcat = Every . conjoin . NonEmpty.toList

instance Monoid Every where
    mempty = Every True
    mconcat = Every . conjoin


-- | Disjunction monoid built with `.||.`.
--
-- Use `property @Some` as an accessor which doesn't leak
-- existential variables.
--
-- Note: monoid laws are satisfied up to `isSuccess` unless one is using
-- `checkCoverage`.
--
data Some = forall p. Testable p => Some { getSome :: p }

instance Testable Some where
    property (Some p) = property p

instance Semigroup Some where
    Some p <> Some p' = Some (p .||. p')
    sconcat = Some . disjoin . NonEmpty.toList

instance Monoid Some where
    mempty = Some False
    mconcat = Some . disjoin

