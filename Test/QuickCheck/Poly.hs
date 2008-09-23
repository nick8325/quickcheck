module Test.QuickCheck.Poly
  ( A, B, C
  , OrdA, OrdB, OrdC
  )
 where

--------------------------------------------------------------------------
-- imports

import Test.QuickCheck.Arbitrary

--------------------------------------------------------------------------
-- polymorphic A, B, C (in Eq)

-- A

newtype A = A{ unA :: Integer }
  deriving ( Eq )

instance Show A where
  showsPrec n (A x) = showsPrec n x

instance Arbitrary A where
  arbitrary    = (A . abs) `fmap` arbitrary
  shrink (A x) = [ A x' | x' <- shrink x, x >= 0 ]

instance CoArbitrary A where
  coarbitrary = coarbitrary . unA

-- B

newtype B = B{ unB :: Integer }
  deriving ( Eq )

instance Show B where
  showsPrec n (B x) = showsPrec n x

instance Arbitrary B where
  arbitrary    = (B . abs) `fmap` arbitrary
  shrink (B x) = [ B x' | x' <- shrink x, x >= 0 ]

instance CoArbitrary B where
  coarbitrary = coarbitrary . unB

-- C

newtype C = C{ unC :: Integer }
  deriving ( Eq )

instance Show C where
  showsPrec n (C x) = showsPrec n x

instance Arbitrary C where
  arbitrary    = (C . abs) `fmap` arbitrary
  shrink (C x) = [ C x' | x' <- shrink x, x >= 0 ]

instance CoArbitrary C where
  coarbitrary = coarbitrary . unC

--------------------------------------------------------------------------
-- polymorphic OrdA, OrdB, OrdC (in Eq, Ord)

-- OrdA

newtype OrdA = OrdA{ unOrdA :: Integer }
  deriving ( Eq, Ord )

instance Show OrdA where
  showsPrec n (OrdA x) = showsPrec n x

instance Arbitrary OrdA where
  arbitrary       = (OrdA . abs) `fmap` arbitrary
  shrink (OrdA x) = [ OrdA x' | x' <- shrink x, x >= 0 ]

instance CoArbitrary OrdA where
  coarbitrary = coarbitrary . unOrdA

-- OrdB

newtype OrdB = OrdB{ unOrdB :: Integer }
  deriving ( Eq, Ord )

instance Show OrdB where
  showsPrec n (OrdB x) = showsPrec n x

instance Arbitrary OrdB where
  arbitrary       = (OrdB . abs) `fmap` arbitrary
  shrink (OrdB x) = [ OrdB x' | x' <- shrink x, x >= 0 ]

instance CoArbitrary OrdB where
  coarbitrary = coarbitrary . unOrdB

-- OrdC

newtype OrdC = OrdC{ unOrdC :: Integer }
  deriving ( Eq, Ord )

instance Show OrdC where
  showsPrec n (OrdC x) = showsPrec n x

instance Arbitrary OrdC where
  arbitrary       = (OrdC . abs) `fmap` arbitrary
  shrink (OrdC x) = [ OrdC x' | x' <- shrink x, x >= 0 ]

instance CoArbitrary OrdC where
  coarbitrary = coarbitrary . unOrdC

--------------------------------------------------------------------------
-- the end.

