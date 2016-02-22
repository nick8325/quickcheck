-- | Type classes for random generation of values.
{-# LANGUAGE CPP #-}
#ifndef NO_GENERICS
{-# LANGUAGE DefaultSignatures, FlexibleContexts, TypeOperators #-}
{-# LANGUAGE FlexibleInstances, KindSignatures, ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
#endif
#ifndef NO_SAFE_HASKELL
{-# LANGUAGE Safe #-}
#endif
module Test.QuickCheck.Arbitrary
  (
  -- * Arbitrary and CoArbitrary classes
    Arbitrary(..)
  , CoArbitrary(..)

  -- ** Helper functions for implementing arbitrary
  , arbitrarySizedIntegral        -- :: Integral a => Gen a
  , arbitrarySizedNatural         -- :: Integral a => Gen a
  , arbitraryBoundedIntegral      -- :: (Bounded a, Integral a) => Gen a
  , arbitrarySizedBoundedIntegral -- :: (Bounded a, Integral a) => Gen a
  , arbitrarySizedFractional      -- :: Fractional a => Gen a
  , arbitraryBoundedRandom        -- :: (Bounded a, Random a) => Gen a
  , arbitraryBoundedEnum          -- :: (Bounded a, Enum a) => Gen a
  -- ** Helper functions for implementing shrink
#ifndef NO_GENERICS
  , genericShrink      -- :: (Generic a, Arbitrary a, RecursivelyShrink (Rep a), GSubterms (Rep a) a) => a -> [a]
  , subterms           -- :: (Generic a, Arbitrary a, GSubterms (Rep a) a) => a -> [a]
  , recursivelyShrink  -- :: (Generic a, RecursivelyShrink (Rep a)) => a -> [a]
  , genericCoarbitrary -- :: (Generic a, GCoArbitrary (Rep a)) => a -> Gen b -> Gen b
#endif
  , shrinkNothing            -- :: a -> [a]
  , shrinkList               -- :: (a -> [a]) -> [a] -> [[a]]
  , shrinkIntegral           -- :: Integral a => a -> [a]
  , shrinkRealFrac           -- :: RealFrac a => a -> [a]
  , shrinkRealFracToInteger  -- :: RealFrac a => a -> [a]
  -- ** Helper functions for implementing coarbitrary
  , coarbitraryIntegral      -- :: Integral a => a -> Gen b -> Gen b
  , coarbitraryReal          -- :: Real a => a -> Gen b -> Gen b
  , coarbitraryShow          -- :: Show a => a -> Gen b -> Gen b
  , coarbitraryEnum          -- :: Enum a => a -> Gen b -> Gen b
  , (><)

  -- ** Generators which use arbitrary
  , vector       -- :: Arbitrary a => Int -> Gen [a]
  , orderedList  -- :: (Ord a, Arbitrary a) => Gen [a]
  , infiniteList -- :: Arbitrary a => Gen [a]
  )
 where

--------------------------------------------------------------------------
-- imports

import Control.Applicative
import Data.Foldable(toList)
import System.Random(Random)
import Test.QuickCheck.Gen
import Test.QuickCheck.Gen.Unsafe

{-
import Data.Generics
  ( (:*:)(..)
  , (:+:)(..)
  , Unit(..)
  )
-}

import Data.Char
  ( chr
  , ord
  , isLower
  , isUpper
  , toLower
  , isDigit
  , isSpace
  )

#ifndef NO_FIXED
import Data.Fixed
  ( Fixed
  , HasResolution
  )
#endif

#ifndef NO_NATURALS
import Numeric.Natural
#endif

import Data.Ratio
  ( Ratio
  , (%)
  , numerator
  , denominator
  )

import Data.Complex
  ( Complex((:+)) )

import Data.List
  ( sort
  , nub
  )

#ifndef NO_NONEMPTY
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Maybe (mapMaybe)
#endif

import Data.Version (Version (..))

import Control.Monad
  ( liftM
  , liftM2
  , liftM3
  , liftM4
  , liftM5
  )

import Data.Int(Int8, Int16, Int32, Int64)
import Data.Word(Word, Word8, Word16, Word32, Word64)

#ifndef NO_GENERICS
import GHC.Generics
#endif

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.IntSet as IntSet
import qualified Data.IntMap as IntMap
import qualified Data.Sequence as Sequence

import qualified Data.Monoid as Monoid

import Data.Functor.Identity
import Data.Functor.Constant

--------------------------------------------------------------------------
-- ** class Arbitrary

-- | Random generation and shrinking of values.
class Arbitrary a where
  -- | A generator for values of the given type.
  arbitrary :: Gen a

  -- | Produces a (possibly) empty list of all the possible
  -- immediate shrinks of the given value. The default implementation
  -- returns the empty list, so will not try to shrink the value.
  --
  -- Most implementations of 'shrink' should try at least three things:
  --
  -- 1. Shrink a term to any of its immediate subterms.
  --
  -- 2. Recursively apply 'shrink' to all immediate subterms.
  --
  -- 3. Type-specific shrinkings such as replacing a constructor by a
  --    simpler constructor.
  --
  -- For example, suppose we have the following implementation of binary trees:
  --
  -- > data Tree a = Nil | Branch a (Tree a) (Tree a)
  --
  -- We can then define 'shrink' as follows:
  --
  -- > shrink Nil = []
  -- > shrink (Branch x l r) =
  -- >   -- shrink Branch to Nil
  -- >   [Nil] ++
  -- >   -- shrink to subterms
  -- >   [l, r] ++
  -- >   -- recursively shrink subterms
  -- >   [Branch x' l' r' | (x', l', r') <- shrink (x, l, r)]
  --
  -- There are a couple of subtleties here:
  --
  -- * QuickCheck tries the shrinking candidates in the order they
  --   appear in the list, so we put more aggressive shrinking steps
  --   (such as replacing the whole tree by @Nil@) before smaller
  --   ones (such as recursively shrinking the subtrees).
  --
  -- * It is tempting to write the last line as
  --   @[Branch x' l' r' | x' <- shrink x, l' <- shrink l, r' <- shrink r]@
  --   but this is the /wrong thing/! It will force QuickCheck to shrink
  --   @x@, @l@ and @r@ in tandem, and shrinking will stop once /one/ of
  --   the three is fully shrunk.
  --
  -- There is a fair bit of boilerplate in the code above.
  -- We can avoid it with the help of some generic functions;
  -- note that these only work on GHC 7.2 and above.
  -- The function 'genericShrink' tries shrinking a term to all of its
  -- subterms and, failing that, recursively shrinks the subterms.
  -- Using it, we can define 'shrink' as:
  --
  -- > shrink x = shrinkToNil x ++ genericShrink x
  -- >   where
  -- >     shrinkToNil Nil = []
  -- >     shrinkToNil (Branch _ l r) = [Nil]
  --
  -- 'genericShrink' is a combination of 'subterms', which shrinks
  -- a term to any of its subterms, and 'recursivelyShrink', which shrinks
  -- all subterms of a term. These may be useful if you need a bit more
  -- control over shrinking than 'genericShrink' gives you.
  --
  -- A final gotcha: we cannot define 'shrink' as simply @'shrink' x = Nil:'genericShrink' x@
  -- as this shrinks @Nil@ to @Nil@, and shrinking will go into an
  -- infinite loop.
  --
  -- If all this leaves you bewildered, you might try @'shrink' = 'genericShrink'@ to begin with,
  -- after deriving @Generic@ for your type. However, if your data type has any
  -- special invariants, you will need to check that 'genericShrink' can't break those invariants.
  shrink :: a -> [a]
  shrink _ = []

#ifndef NO_GENERICS
-- | Shrink a term to any of its immediate subterms,
-- and also recursively shrink all subterms.
genericShrink :: (Generic a, RecursivelyShrink (Rep a), GSubterms (Rep a) a) => a -> [a]
genericShrink x = subterms x ++ recursivelyShrink x

-- | Recursively shrink all immediate subterms.
recursivelyShrink :: (Generic a, RecursivelyShrink (Rep a)) => a -> [a]
recursivelyShrink = map to . grecursivelyShrink . from

class RecursivelyShrink f where
  grecursivelyShrink :: f a -> [f a]

instance (RecursivelyShrink f, RecursivelyShrink g) => RecursivelyShrink (f :*: g) where
  grecursivelyShrink (x :*: y) =
    [x' :*: y | x' <- grecursivelyShrink x] ++
    [x :*: y' | y' <- grecursivelyShrink y]

instance (RecursivelyShrink f, RecursivelyShrink g) => RecursivelyShrink (f :+: g) where
  grecursivelyShrink (L1 x) = map L1 (grecursivelyShrink x)
  grecursivelyShrink (R1 x) = map R1 (grecursivelyShrink x)

instance RecursivelyShrink f => RecursivelyShrink (M1 i c f) where
  grecursivelyShrink (M1 x) = map M1 (grecursivelyShrink x)

instance Arbitrary a => RecursivelyShrink (K1 i a) where
  grecursivelyShrink (K1 x) = map K1 (shrink x)

instance RecursivelyShrink U1 where
  grecursivelyShrink U1 = []

instance RecursivelyShrink V1 where
  -- The empty type can't be shrunk to anything.
  grecursivelyShrink _ = []


-- | All immediate subterms of a term.
subterms :: (Generic a, GSubterms (Rep a) a) => a -> [a]
subterms = gSubterms . from


class GSubterms f a where
  -- | Provides the immediate subterms of a term that are of the same type
  -- as the term itself.
  --
  -- Requires a constructor to be stripped off; this means it skips through
  -- @M1@ wrappers and returns @[]@ on everything that's not `(:*:)` or `(:+:)`.
  --
  -- Once a `(:*:)` or `(:+:)` constructor has been reached, this function
  -- delegates to `gSubtermsIncl` to return the immediately next constructor
  -- available.
  gSubterms :: f a -> [a]

instance GSubterms V1 a where
  -- The empty type can't be shrunk to anything.
  gSubterms _ = []

instance GSubterms U1 a where
  gSubterms U1 = []

instance (GSubtermsIncl f a, GSubtermsIncl g a) => GSubterms (f :*: g) a where
  gSubterms (l :*: r) = gSubtermsIncl l ++ gSubtermsIncl r

instance (GSubtermsIncl f a, GSubtermsIncl g a) => GSubterms (f :+: g) a where
  gSubterms (L1 x) = gSubtermsIncl x
  gSubterms (R1 x) = gSubtermsIncl x

instance GSubterms f a => GSubterms (M1 i c f) a where
  gSubterms (M1 x) = gSubterms x

instance GSubterms (K1 i a) b where
  gSubterms (K1 _) = []


class GSubtermsIncl f a where
  -- | Provides the immediate subterms of a term that are of the same type
  -- as the term itself.
  --
  -- In contrast to `gSubterms`, this returns the immediate next constructor
  -- available.
  gSubtermsIncl :: f a -> [a]

instance GSubtermsIncl V1 a where
  -- The empty type can't be shrunk to anything.
  gSubtermsIncl _ = []

instance GSubtermsIncl U1 a where
  gSubtermsIncl U1 = []

instance (GSubtermsIncl f a, GSubtermsIncl g a) => GSubtermsIncl (f :*: g) a where
  gSubtermsIncl (l :*: r) = gSubtermsIncl l ++ gSubtermsIncl r

instance (GSubtermsIncl f a, GSubtermsIncl g a) => GSubtermsIncl (f :+: g) a where
  gSubtermsIncl (L1 x) = gSubtermsIncl x
  gSubtermsIncl (R1 x) = gSubtermsIncl x

instance GSubtermsIncl f a => GSubtermsIncl (M1 i c f) a where
  gSubtermsIncl (M1 x) = gSubtermsIncl x

type family Same a b where
  Same a a = 'True
  Same a b = 'False

-- An auxiliary class to handle K1 without overlapping
-- instances.
class same ~ Same a b => GSubtermsInclK1 (same :: Bool) a b where
  gSubtermsInclK1 :: K1 i a b -> [b]

-- This is the important case: We've found a term of the same type.
instance a ~ b => GSubtermsInclK1 'True a b where
  gSubtermsInclK1 (K1 x) = [x]

instance Same a b ~ 'False => GSubtermsInclK1 'False a b where
  gSubtermsInclK1 (K1 _) = []

instance GSubtermsInclK1 same a b => GSubtermsIncl (K1 i a) b where
  gSubtermsIncl = gSubtermsInclK1

#endif

-- instances

instance (CoArbitrary a, Arbitrary b) => Arbitrary (a -> b) where
  arbitrary = promote (`coarbitrary` arbitrary)

instance Arbitrary () where
  arbitrary = return ()

instance Arbitrary Bool where
  arbitrary = choose (False,True)
  shrink True = [False]
  shrink False = []

instance Arbitrary Ordering where
  arbitrary = elements [LT, EQ, GT]
  shrink GT = [EQ, LT]
  shrink LT = [EQ]
  shrink EQ = []

instance Arbitrary a => Arbitrary (Maybe a) where
  arbitrary = frequency [(1, return Nothing), (3, liftM Just arbitrary)]

  shrink (Just x) = Nothing : [ Just x' | x' <- shrink x ]
  shrink _        = []

instance (Arbitrary a, Arbitrary b) => Arbitrary (Either a b) where
  arbitrary = oneof [liftM Left arbitrary, liftM Right arbitrary]

  shrink (Left x)  = [ Left  x' | x' <- shrink x ]
  shrink (Right y) = [ Right y' | y' <- shrink y ]

instance Arbitrary a => Arbitrary [a] where
  arbitrary = listOf arbitrary
  shrink xs = shrinkList shrink xs

#ifndef NO_NONEMPTY
instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = liftM2 (:|) arbitrary arbitrary
  shrink (x :| xs) = mapMaybe nonEmpty . shrinkList shrink $ x : xs
#endif

-- | Shrink a list of values given a shrinking function for individual values.
shrinkList :: (a -> [a]) -> [a] -> [[a]]
shrinkList shr xs = concat [ removes k n xs | k <- takeWhile (>0) (iterate (`div`2) n) ]
                 ++ shrinkOne xs
 where
  n = length xs

  shrinkOne []     = []
  shrinkOne (x:xs) = [ x':xs | x'  <- shr x ]
                  ++ [ x:xs' | xs' <- shrinkOne xs ]

  removes k n xs
    | k > n     = []
    | null xs2  = [[]]
    | otherwise = xs2 : map (xs1 ++) (removes k (n-k) xs2)
   where
    xs1 = take k xs
    xs2 = drop k xs

{-
  -- "standard" definition for lists:
  shrink []     = []
  shrink (x:xs) = [ xs ]
               ++ [ x:xs' | xs' <- shrink xs ]
               ++ [ x':xs | x'  <- shrink x ]
-}

instance Integral a => Arbitrary (Ratio a) where
  arbitrary = arbitrarySizedFractional
  shrink    = shrinkRealFracToInteger

instance (RealFloat a, Arbitrary a) => Arbitrary (Complex a) where
  arbitrary = liftM2 (:+) arbitrary arbitrary
  shrink (x :+ y) = [ x' :+ y | x' <- shrink x ] ++
                    [ x :+ y' | y' <- shrink y ]

#ifndef NO_FIXED
instance HasResolution a => Arbitrary (Fixed a) where
  arbitrary = arbitrarySizedFractional
  shrink    = shrinkRealFrac
#endif

instance (Arbitrary a, Arbitrary b)
      => Arbitrary (a,b)
 where
  arbitrary = liftM2 (,) arbitrary arbitrary

  shrink (x, y) =
       [ (x', y) | x' <- shrink x ]
    ++ [ (x, y') | y' <- shrink y ]

instance (Arbitrary a, Arbitrary b, Arbitrary c)
      => Arbitrary (a,b,c)
 where
  arbitrary = liftM3 (,,) arbitrary arbitrary arbitrary

  shrink (x, y, z) =
    [ (x', y', z')
    | (x', (y', z')) <- shrink (x, (y, z)) ]

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
      => Arbitrary (a,b,c,d)
 where
  arbitrary = liftM4 (,,,) arbitrary arbitrary arbitrary arbitrary

  shrink (w, x, y, z) =
    [ (w', x', y', z')
    | (w', (x', (y', z'))) <- shrink (w, (x, (y, z))) ]

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e)
      => Arbitrary (a,b,c,d,e)
 where
  arbitrary = liftM5 (,,,,) arbitrary arbitrary arbitrary arbitrary arbitrary

  shrink (v, w, x, y, z) =
    [ (v', w', x', y', z')
    | (v', (w', (x', (y', z')))) <- shrink (v, (w, (x, (y, z)))) ]

-- typical instance for primitive (numerical) types

instance Arbitrary Integer where
  arbitrary = arbitrarySizedIntegral
  shrink    = shrinkIntegral

#ifndef NO_NATURALS
instance Arbitrary Natural where
  arbitrary = arbitrarySizedNatural
  shrink    = shrinkIntegral
#endif

instance Arbitrary Int where
  arbitrary = arbitrarySizedIntegral
  shrink    = shrinkIntegral

instance Arbitrary Int8 where
  arbitrary = arbitrarySizedBoundedIntegral
  shrink    = shrinkIntegral

instance Arbitrary Int16 where
  arbitrary = arbitrarySizedBoundedIntegral
  shrink    = shrinkIntegral

instance Arbitrary Int32 where
  arbitrary = arbitrarySizedBoundedIntegral
  shrink    = shrinkIntegral

instance Arbitrary Int64 where
  arbitrary = arbitrarySizedBoundedIntegral
  shrink    = shrinkIntegral

instance Arbitrary Word where
  arbitrary = arbitrarySizedBoundedIntegral
  shrink    = shrinkIntegral

instance Arbitrary Word8 where
  arbitrary = arbitrarySizedBoundedIntegral
  shrink    = shrinkIntegral

instance Arbitrary Word16 where
  arbitrary = arbitrarySizedBoundedIntegral
  shrink    = shrinkIntegral

instance Arbitrary Word32 where
  arbitrary = arbitrarySizedBoundedIntegral
  shrink    = shrinkIntegral

instance Arbitrary Word64 where
  arbitrary = arbitrarySizedBoundedIntegral
  shrink    = shrinkIntegral

instance Arbitrary Char where
  arbitrary = chr `fmap` oneof [choose (0,127), choose (0,255)]
  shrink c  = filter (<. c) $ nub
            $ ['a','b','c']
           ++ [ toLower c | isUpper c ]
           ++ ['A','B','C']
           ++ ['1','2','3']
           ++ [' ','\n']
   where
    a <. b  = stamp a < stamp b
    stamp a = ( (not (isLower a)
              , not (isUpper a)
              , not (isDigit a))
              , (not (a==' ')
              , not (isSpace a)
              , a)
              )

instance Arbitrary Float where
  arbitrary = arbitrarySizedFractional
  shrink    = shrinkRealFrac

instance Arbitrary Double where
  arbitrary = arbitrarySizedFractional
  shrink    = shrinkRealFrac

-- Arbitrary instances for container types
instance (Ord a, Arbitrary a) => Arbitrary (Set.Set a) where
  arbitrary = fmap Set.fromList arbitrary
  shrink = map Set.fromList . shrink . Set.toList
instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (Map.Map k v) where
  arbitrary = fmap Map.fromList arbitrary
  shrink = map Map.fromList . shrink . Map.toList
instance Arbitrary IntSet.IntSet where
  arbitrary = fmap IntSet.fromList arbitrary
  shrink = map IntSet.fromList . shrink . IntSet.toList
instance Arbitrary a => Arbitrary (IntMap.IntMap a) where
  arbitrary = fmap IntMap.fromList arbitrary
  shrink = map IntMap.fromList . shrink . IntMap.toList
instance Arbitrary a => Arbitrary (Sequence.Seq a) where
  arbitrary = fmap Sequence.fromList arbitrary
  shrink = map Sequence.fromList . shrink . toList

-- Arbitrary instance for Ziplist
instance Arbitrary a => Arbitrary (ZipList a) where
  arbitrary = fmap ZipList arbitrary
  shrink = map ZipList . shrink . getZipList

-- Arbitrary instance for transformers' Functors
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary
  shrink = map Identity . shrink . runIdentity

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = fmap Constant arbitrary
  shrink = map Constant . shrink . getConstant

-- Arbitrary instance for Const
instance Arbitrary a => Arbitrary (Const a b) where
  arbitrary = fmap Const arbitrary
  shrink = map Const . shrink . getConst

-- Arbitrary instances for Monoid
instance Arbitrary a => Arbitrary (Monoid.Dual a) where
  arbitrary = fmap Monoid.Dual arbitrary
  shrink = map Monoid.Dual . shrink . Monoid.getDual

instance (Arbitrary a, CoArbitrary a) => Arbitrary (Monoid.Endo a) where
  arbitrary = fmap Monoid.Endo arbitrary
  shrink = map Monoid.Endo . shrink . Monoid.appEndo

instance Arbitrary Monoid.All where
  arbitrary = fmap Monoid.All arbitrary
  shrink = map Monoid.All . shrink . Monoid.getAll

instance Arbitrary Monoid.Any where
  arbitrary = fmap Monoid.Any arbitrary
  shrink = map Monoid.Any . shrink . Monoid.getAny

instance Arbitrary a => Arbitrary (Monoid.Sum a) where
  arbitrary = fmap Monoid.Sum arbitrary
  shrink = map Monoid.Sum . shrink . Monoid.getSum

instance Arbitrary a => Arbitrary (Monoid.Product a) where
  arbitrary = fmap Monoid.Product  arbitrary
  shrink = map Monoid.Product  . shrink . Monoid.getProduct

instance Arbitrary a => Arbitrary (Monoid.First a) where
  arbitrary = fmap Monoid.First arbitrary
  shrink = map Monoid.First . shrink . Monoid.getFirst

instance Arbitrary a => Arbitrary (Monoid.Last a) where
  arbitrary = fmap Monoid.Last arbitrary
  shrink = map Monoid.Last . shrink . Monoid.getLast

#if defined(MIN_VERSION_base)
#if MIN_VERSION_base(4,8,0)
instance Arbitrary (f a) => Arbitrary (Monoid.Alt f a) where
  arbitrary = fmap Monoid.Alt arbitrary
  shrink = map Monoid.Alt . shrink . Monoid.getAlt
#endif
#endif

-- | Generates 'Version' with non-empty non-negative @versionBranch@, and empty @versionTags@
instance Arbitrary Version where
  arbitrary = sized $ \n ->
    do k <- choose (0, log2 n)
       xs <- vectorOf (k+1) arbitrarySizedNatural
       return (Version xs [])
    where
      log2 :: Int -> Int
      log2 n | n <= 1 = 0
             | otherwise = 1 + log2 (n `div` 2)

  shrink (Version xs _) =
    [ Version xs' []
    | xs' <- shrink xs
    , length xs' > 0
    , all (>=0) xs'
    ]

-- ** Helper functions for implementing arbitrary

-- | Generates an integral number. The number can be positive or negative
-- and its maximum absolute value depends on the size parameter.
arbitrarySizedIntegral :: Integral a => Gen a
arbitrarySizedIntegral =
  sized $ \n ->
  inBounds fromInteger (choose (-toInteger n, toInteger n))

-- | Generates a natural number. The number's maximum value depends on
-- the size parameter.
arbitrarySizedNatural :: Integral a => Gen a
arbitrarySizedNatural =
  sized $ \n ->
  inBounds fromInteger (choose (0, toInteger n))

inBounds :: Integral a => (Integer -> a) -> Gen Integer -> Gen a
inBounds fi g = fmap fi (g `suchThat` (\x -> toInteger (fi x) == x))

-- | Generates a fractional number. The number can be positive or negative
-- and its maximum absolute value depends on the size parameter.
arbitrarySizedFractional :: Fractional a => Gen a
arbitrarySizedFractional =
  sized $ \n ->
    let n' = toInteger n in
      do a <- choose ((-n') * precision, n' * precision)
         b <- choose (1, precision)
         return (fromRational (a % b))
 where
  precision = 9999999999999 :: Integer

-- Useful for getting at minBound and maxBound without having to
-- fiddle around with asTypeOf.
withBounds :: Bounded a => (a -> a -> Gen a) -> Gen a
withBounds k = k minBound maxBound

-- | Generates an integral number. The number is chosen uniformly from
-- the entire range of the type. You may want to use
-- 'arbitrarySizedBoundedIntegral' instead.
arbitraryBoundedIntegral :: (Bounded a, Integral a) => Gen a
arbitraryBoundedIntegral =
  withBounds $ \mn mx ->
  do n <- choose (toInteger mn, toInteger mx)
     return (fromInteger n)

-- | Generates an element of a bounded type. The element is
-- chosen from the entire range of the type.
arbitraryBoundedRandom :: (Bounded a, Random a) => Gen a
arbitraryBoundedRandom = choose (minBound,maxBound)

-- | Generates an element of a bounded enumeration.
arbitraryBoundedEnum :: (Bounded a, Enum a) => Gen a
arbitraryBoundedEnum =
  withBounds $ \mn mx ->
  do n <- choose (fromEnum mn, fromEnum mx)
     return (toEnum n)

-- | Generates an integral number from a bounded domain. The number is
-- chosen from the entire range of the type, but small numbers are
-- generated more often than big numbers. Inspired by demands from
-- Phil Wadler.
arbitrarySizedBoundedIntegral :: (Bounded a, Integral a) => Gen a
arbitrarySizedBoundedIntegral =
  withBounds $ \mn mx ->
  sized $ \s ->
    do let bits n | n `quot` 2 == 0 = 0
                  | otherwise = 1 + bits (n `quot` 2)
           k  = 2^(s*(bits mn `max` bits mx `max` 40) `div` 100)
       n <- choose (toInteger mn `max` (-k), toInteger mx `min` k)
       return (fromInteger n)

-- ** Helper functions for implementing shrink

-- | Returns no shrinking alternatives.
shrinkNothing :: a -> [a]
shrinkNothing _ = []

-- | Shrink an integral number.
shrinkIntegral :: Integral a => a -> [a]
shrinkIntegral x =
  nub $
  [ -x
  | x < 0, -x > x
  ] ++
  [ x'
  | x' <- takeWhile (<< x) (0:[ x - i | i <- tail (iterate (`quot` 2) x) ])
  ]
 where
   -- a << b is "morally" abs a < abs b, but taking care of overflow.
   a << b = case (a >= 0, b >= 0) of
            (True,  True)  -> a < b
            (False, False) -> a > b
            (True,  False) -> a + b < 0
            (False, True)  -> a + b > 0

-- | Shrink a fraction, but only shrink to integral values.
shrinkRealFracToInteger :: RealFrac a => a -> [a]
shrinkRealFracToInteger x =
  nub $
  [ -x
  | x < 0
  ] ++
  map fromInteger (shrinkIntegral (truncate x))

-- | Shrink a fraction.
shrinkRealFrac :: RealFrac a => a -> [a]
shrinkRealFrac x =
  nub $
  shrinkRealFracToInteger x ++
  [ x - x'
  | x' <- take 20 (iterate (/ 2) x)
  , (x - x') << x ]
 where
  a << b = abs a < abs b

--------------------------------------------------------------------------
-- ** CoArbitrary

#ifndef NO_GENERICS
-- | Used for random generation of functions.
--
-- If you are using a recent GHC, there is a default definition of
-- 'coarbitrary' using 'genericCoarbitrary', so if your type has a
-- 'Generic' instance it's enough to say
--
-- > instance CoArbitrary MyType
--
-- You should only use 'genericCoarbitrary' for data types where
-- equality is structural, i.e. if you can't have two different
-- representations of the same value. An example where it's not
-- safe is sets implemented using binary search trees: the same
-- set can be represented as several different trees.
-- Here you would have to explicitly define
-- @coarbitrary s = coarbitrary (toList s)@.
#else
-- | Used for random generation of functions.
#endif
class CoArbitrary a where
  -- | Used to generate a function of type @a -> b@.
  -- The first argument is a value, the second a generator.
  -- You should use 'variant' to perturb the random generator;
  -- the goal is that different values for the first argument will
  -- lead to different calls to 'variant'. An example will help:
  --
  -- @
  -- instance CoArbitrary a => CoArbitrary [a] where
  --   coarbitrary []     = 'variant' 0
  --   coarbitrary (x:xs) = 'variant' 1 . coarbitrary (x,xs)
  -- @
  coarbitrary :: a -> Gen b -> Gen b
#ifndef NO_GENERICS
  default coarbitrary :: (Generic a, GCoArbitrary (Rep a)) => a -> Gen b -> Gen b
  coarbitrary = genericCoarbitrary

-- | Generic CoArbitrary implementation.
genericCoarbitrary :: (Generic a, GCoArbitrary (Rep a)) => a -> Gen b -> Gen b
genericCoarbitrary = gCoarbitrary . from

class GCoArbitrary f where
  gCoarbitrary :: f a -> Gen b -> Gen b

instance GCoArbitrary U1 where
  gCoarbitrary U1 = id

instance (GCoArbitrary f, GCoArbitrary g) => GCoArbitrary (f :*: g) where
  -- Like the instance for tuples.
  gCoarbitrary (l :*: r) = gCoarbitrary l . gCoarbitrary r

instance (GCoArbitrary f, GCoArbitrary g) => GCoArbitrary (f :+: g) where
  -- Like the instance for Either.
  gCoarbitrary (L1 x) = variant 0 . gCoarbitrary x
  gCoarbitrary (R1 x) = variant 1 . gCoarbitrary x

instance GCoArbitrary f => GCoArbitrary (M1 i c f) where
  gCoarbitrary (M1 x) = gCoarbitrary x

instance CoArbitrary a => GCoArbitrary (K1 i a) where
  gCoarbitrary (K1 x) = coarbitrary x
#endif

{-# DEPRECATED (><) "Use ordinary function composition instead" #-}
-- | Combine two generator perturbing functions, for example the
-- results of calls to 'variant' or 'coarbitrary'.
(><) :: (Gen a -> Gen a) -> (Gen a -> Gen a) -> (Gen a -> Gen a)
(><) = (.)

instance (Arbitrary a, CoArbitrary b) => CoArbitrary (a -> b) where
  coarbitrary f gen =
    do xs <- arbitrary
       coarbitrary (map f xs) gen

instance CoArbitrary () where
  coarbitrary _ = id

instance CoArbitrary Bool where
  coarbitrary False = variant 0
  coarbitrary True  = variant 1

instance CoArbitrary Ordering where
  coarbitrary GT = variant 0
  coarbitrary EQ = variant 1
  coarbitrary LT = variant 2

instance CoArbitrary a => CoArbitrary (Maybe a) where
  coarbitrary Nothing  = variant 0
  coarbitrary (Just x) = variant 1 . coarbitrary x

instance (CoArbitrary a, CoArbitrary b) => CoArbitrary (Either a b) where
  coarbitrary (Left x)  = variant 0 . coarbitrary x
  coarbitrary (Right y) = variant 1 . coarbitrary y

instance CoArbitrary a => CoArbitrary [a] where
  coarbitrary []     = variant 0
  coarbitrary (x:xs) = variant 1 . coarbitrary (x,xs)

#ifndef NO_NONEMPTY
instance CoArbitrary a => CoArbitrary (NonEmpty a) where
  coarbitrary (x :| xs) = coarbitrary (x, xs)
#endif

instance (Integral a, CoArbitrary a) => CoArbitrary (Ratio a) where
  coarbitrary r = coarbitrary (numerator r,denominator r)

#ifndef NO_FIXED
instance HasResolution a => CoArbitrary (Fixed a) where
  coarbitrary = coarbitraryReal
#endif

instance (RealFloat a, CoArbitrary a) => CoArbitrary (Complex a) where
  coarbitrary (x :+ y) = coarbitrary x . coarbitrary y

instance (CoArbitrary a, CoArbitrary b)
      => CoArbitrary (a,b)
 where
  coarbitrary (x,y) = coarbitrary x
                    . coarbitrary y

instance (CoArbitrary a, CoArbitrary b, CoArbitrary c)
      => CoArbitrary (a,b,c)
 where
  coarbitrary (x,y,z) = coarbitrary x
                      . coarbitrary y
                      . coarbitrary z

instance (CoArbitrary a, CoArbitrary b, CoArbitrary c, CoArbitrary d)
      => CoArbitrary (a,b,c,d)
 where
  coarbitrary (x,y,z,v) = coarbitrary x
                        . coarbitrary y
                        . coarbitrary z
                        . coarbitrary v

instance (CoArbitrary a, CoArbitrary b, CoArbitrary c, CoArbitrary d, CoArbitrary e)
      => CoArbitrary (a,b,c,d,e)
 where
  coarbitrary (x,y,z,v,w) = coarbitrary x
                          . coarbitrary y
                          . coarbitrary z
                          . coarbitrary v
                          . coarbitrary w

-- typical instance for primitive (numerical) types

instance CoArbitrary Integer where
  coarbitrary = coarbitraryIntegral

#ifndef NO_NATURALS
instance CoArbitrary Natural where
  coarbitrary = coarbitraryIntegral
#endif

instance CoArbitrary Int where
  coarbitrary = coarbitraryIntegral

instance CoArbitrary Int8 where
  coarbitrary = coarbitraryIntegral

instance CoArbitrary Int16 where
  coarbitrary = coarbitraryIntegral

instance CoArbitrary Int32 where
  coarbitrary = coarbitraryIntegral

instance CoArbitrary Int64 where
  coarbitrary = coarbitraryIntegral

instance CoArbitrary Word where
  coarbitrary = coarbitraryIntegral

instance CoArbitrary Word8 where
  coarbitrary = coarbitraryIntegral

instance CoArbitrary Word16 where
  coarbitrary = coarbitraryIntegral

instance CoArbitrary Word32 where
  coarbitrary = coarbitraryIntegral

instance CoArbitrary Word64 where
  coarbitrary = coarbitraryIntegral

instance CoArbitrary Char where
  coarbitrary = coarbitrary . ord

instance CoArbitrary Float where
  coarbitrary = coarbitraryReal

instance CoArbitrary Double where
  coarbitrary = coarbitraryReal

-- Coarbitrary instances for container types
instance CoArbitrary a => CoArbitrary (Set.Set a) where
  coarbitrary = coarbitrary. Set.toList
instance (CoArbitrary k, CoArbitrary v) => CoArbitrary (Map.Map k v) where
  coarbitrary = coarbitrary . Map.toList
instance CoArbitrary IntSet.IntSet where
  coarbitrary = coarbitrary . IntSet.toList
instance CoArbitrary a => CoArbitrary (IntMap.IntMap a) where
  coarbitrary = coarbitrary . IntMap.toList
instance CoArbitrary a => CoArbitrary (Sequence.Seq a) where
  coarbitrary = coarbitrary . toList

-- CoArbitrary instance for Ziplist
instance CoArbitrary a => CoArbitrary (ZipList a) where
  coarbitrary = coarbitrary . getZipList

-- CoArbitrary instance for transformers' Functors
instance CoArbitrary a => CoArbitrary (Identity a) where
  coarbitrary = coarbitrary . runIdentity

instance CoArbitrary a => CoArbitrary (Constant a b) where
  coarbitrary = coarbitrary . getConstant

-- CoArbitrary instance for Const
instance CoArbitrary a => CoArbitrary (Const a b) where
  coarbitrary = coarbitrary . getConst

-- CoArbitrary instances for Monoid
instance CoArbitrary a => CoArbitrary (Monoid.Dual a) where
  coarbitrary = coarbitrary . Monoid.getDual

instance (Arbitrary a, CoArbitrary a) => CoArbitrary (Monoid.Endo a) where
  coarbitrary = coarbitrary . Monoid.appEndo

instance CoArbitrary Monoid.All where
  coarbitrary = coarbitrary . Monoid.getAll

instance CoArbitrary Monoid.Any where
  coarbitrary = coarbitrary . Monoid.getAny

instance CoArbitrary a => CoArbitrary (Monoid.Sum a) where
  coarbitrary = coarbitrary . Monoid.getSum

instance CoArbitrary a => CoArbitrary (Monoid.Product a) where
  coarbitrary = coarbitrary . Monoid.getProduct

instance CoArbitrary a => CoArbitrary (Monoid.First a) where
  coarbitrary = coarbitrary . Monoid.getFirst

instance CoArbitrary a => CoArbitrary (Monoid.Last a) where
  coarbitrary = coarbitrary . Monoid.getLast

#if defined(MIN_VERSION_base)
#if MIN_VERSION_base(4,8,0)
instance CoArbitrary (f a) => CoArbitrary (Monoid.Alt f a) where
  coarbitrary = coarbitrary . Monoid.getAlt
#endif
#endif

instance CoArbitrary Version where
  coarbitrary (Version a b) = coarbitrary (a, b)

-- ** Helpers for implementing coarbitrary

-- | A 'coarbitrary' implementation for integral numbers.
coarbitraryIntegral :: Integral a => a -> Gen b -> Gen b
coarbitraryIntegral = variant

-- | A 'coarbitrary' implementation for real numbers.
coarbitraryReal :: Real a => a -> Gen b -> Gen b
coarbitraryReal x = coarbitrary (toRational x)

-- | 'coarbitrary' helper for lazy people :-).
coarbitraryShow :: Show a => a -> Gen b -> Gen b
coarbitraryShow x = coarbitrary (show x)

-- | A 'coarbitrary' implementation for enums.
coarbitraryEnum :: Enum a => a -> Gen b -> Gen b
coarbitraryEnum = variant . fromEnum

--------------------------------------------------------------------------
-- ** arbitrary generators

-- these are here and not in Gen because of the Arbitrary class constraint

-- | Generates a list of a given length.
vector :: Arbitrary a => Int -> Gen [a]
vector k = vectorOf k arbitrary

-- | Generates an ordered list.
orderedList :: (Ord a, Arbitrary a) => Gen [a]
orderedList = sort `fmap` arbitrary

-- | Generate an infinite list.
infiniteList :: Arbitrary a => Gen [a]
infiniteList = infiniteListOf arbitrary

--------------------------------------------------------------------------
-- the end.
