-- | Type classes for random generation of values.
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
#ifndef NO_GENERICS
{-# LANGUAGE DefaultSignatures, FlexibleContexts, TypeOperators #-}
{-# LANGUAGE FlexibleInstances, KindSignatures, ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
#if __GLASGOW_HASKELL__ >= 710
#define OVERLAPPING_ {-# OVERLAPPING #-}
#else
{-# LANGUAGE OverlappingInstances  #-}
#define OVERLAPPING_
#endif
#endif
#ifndef NO_POLYKINDS
{-# LANGUAGE PolyKinds #-}
#endif
#ifndef NO_SAFE_HASKELL
{-# LANGUAGE Safe #-}
#endif
module Test.QuickCheck.Arbitrary
  (
  -- * Arbitrary and CoArbitrary classes
    Arbitrary(..)
  , CoArbitrary(..)

  -- ** Unary and Binary classes
  , Arbitrary1(..)
  , arbitrary1
  , shrink1
  , Arbitrary2(..)
  , arbitrary2
  , shrink2

  -- ** Helper functions for implementing arbitrary
  , arbitrarySizedIntegral        -- :: Integral a => Gen a
  , arbitrarySizedNatural         -- :: Integral a => Gen a
  , arbitraryBoundedIntegral      -- :: (Bounded a, Integral a) => Gen a
  , arbitrarySizedBoundedIntegral -- :: (Bounded a, Integral a) => Gen a
  , arbitrarySizedFractional      -- :: Fractional a => Gen a
  , arbitraryBoundedRandom        -- :: (Bounded a, Random a) => Gen a
  , arbitraryBoundedEnum          -- :: (Bounded a, Enum a) => Gen a
  -- ** Generators for various kinds of character
  , arbitraryUnicodeChar   -- :: Gen Char
  , arbitraryASCIIChar     -- :: Gen Char
  , arbitraryPrintableChar -- :: Gen Char
  -- ** Helper functions for implementing shrink
#ifndef NO_GENERICS
  , genericShrink      -- :: (Generic a, Arbitrary a, RecursivelyShrink (Rep a), GSubterms (Rep a) a) => a -> [a]
  , subterms           -- :: (Generic a, Arbitrary a, GSubterms (Rep a) a) => a -> [a]
  , recursivelyShrink  -- :: (Generic a, RecursivelyShrink (Rep a)) => a -> [a]
  , genericCoarbitrary -- :: (Generic a, GCoArbitrary (Rep a)) => a -> Gen b -> Gen b
#endif
  , shrinkNothing            -- :: a -> [a]
  , shrinkList               -- :: (a -> [a]) -> [a] -> [[a]]
  , shrinkMap                -- :: Arbitrary a -> (a -> b) -> (b -> a) -> b -> [b]
  , shrinkMapBy              -- :: (a -> b) -> (b -> a) -> (a -> [a]) -> b -> [b]
  , shrinkIntegral           -- :: Integral a => a -> [a]
  , shrinkRealFrac           -- :: RealFrac a => a -> [a]
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
import Test.QuickCheck.Random
import Test.QuickCheck.Gen.Unsafe

{-
import Data.Generics
  ( (:*:)(..)
  , (:+:)(..)
  , Unit(..)
  )
-}

import Data.Char
  ( ord
  , isLower
  , isUpper
  , toLower
  , isDigit
  , isSpace
  , isPrint
  , generalCategory
  , GeneralCategory(..)
  )

#ifndef NO_FIXED
import Data.Fixed
  ( Fixed
  , HasResolution
  )
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
import System.Exit (ExitCode(..))
#ifndef NO_CTYPES
import Foreign.C.Types
#endif

#ifndef NO_GENERICS
import GHC.Generics
#endif

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.IntSet as IntSet
import qualified Data.IntMap as IntMap
import qualified Data.Sequence as Sequence

import qualified Data.Monoid as Monoid

#ifndef NO_TRANSFORMERS
import Data.Functor.Identity
import Data.Functor.Constant
import Data.Functor.Compose
import Data.Functor.Product
#endif

--------------------------------------------------------------------------
-- ** class Arbitrary

-- | Random generation and shrinking of values.
--
-- QuickCheck provides @Arbitrary@ instances for most types in @base@,
-- except those which incur extra dependencies.
-- For a wider range of @Arbitrary@ instances see the
-- <http://hackage.haskell.org/package/quickcheck-instances quickcheck-instances>
-- package.
class Arbitrary a where
  -- | A generator for values of the given type.
  --
  -- It is worth spending time thinking about what sort of test data
  -- you want - good generators are often the difference between
  -- finding bugs and not finding them. You can use 'sample',
  -- 'label' and 'classify' to check the quality of your test data.
  --
  -- There is no generic @arbitrary@ implementation included because we don't
  -- know how to make a high-quality one. If you want one, consider using the
  -- <http://hackage.haskell.org/package/testing-feat testing-feat> package.
  arbitrary :: Gen a

  -- | Produces a (possibly) empty list of all the possible
  -- immediate shrinks of the given value.
  --
  -- The default implementation returns the empty list, so will not try to
  -- shrink the value. If your data type has no special invariants, you can
  -- enable shrinking by defining @shrink = 'genericShrink'@, but by customising
  -- the behaviour of @shrink@ you can often get simpler counterexamples.
  --
  -- Most implementations of 'shrink' should try at least three things:
  --
  -- 1. Shrink a term to any of its immediate subterms.
  --    You can use 'subterms' to do this.
  --
  -- 2. Recursively apply 'shrink' to all immediate subterms.
  --    You can use 'recursivelyShrink' to do this.
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
  -- We can avoid it with the help of some generic functions.
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

-- | Lifting of the 'Arbitrary' class to unary type constructors.
class Arbitrary1 f where
  liftArbitrary :: Gen a -> Gen (f a)
  liftShrink    :: (a -> [a]) -> f a -> [f a]
  liftShrink _ _ = []

arbitrary1 :: (Arbitrary1 f, Arbitrary a) => Gen (f a)
arbitrary1 = liftArbitrary arbitrary

shrink1 :: (Arbitrary1 f, Arbitrary a) => f a -> [f a]
shrink1 = liftShrink shrink

-- | Lifting of the 'Arbitrary' class to binary type constructors.
class Arbitrary2 f where
  liftArbitrary2 :: Gen a -> Gen b -> Gen (f a b)
  liftShrink2    :: (a -> [a]) -> (b -> [b]) -> f a b -> [f a b]
  liftShrink2 _ _ _ = []

arbitrary2 :: (Arbitrary2 f, Arbitrary a, Arbitrary b) => Gen (f a b)
arbitrary2 = liftArbitrary2 arbitrary arbitrary

shrink2 :: (Arbitrary2 f, Arbitrary a, Arbitrary b) => f a b -> [f a b]
shrink2 = liftShrink2 shrink shrink

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

-- This is the important case: We've found a term of the same type.
instance OVERLAPPING_ GSubtermsIncl (K1 i a) a where
  gSubtermsIncl (K1 x) = [x]

instance OVERLAPPING_ GSubtermsIncl (K1 i a) b where
  gSubtermsIncl (K1 _) = []

#endif

-- instances

instance (CoArbitrary a) => Arbitrary1 ((->) a) where
  liftArbitrary arbB = promote (`coarbitrary` arbB)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (a -> b) where
  arbitrary = arbitrary1

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

instance Arbitrary1 Maybe where
  liftArbitrary arb = frequency [(1, return Nothing), (3, liftM Just arb)]

  liftShrink shr (Just x) = Nothing : [ Just x' | x' <- shr x ]
  liftShrink _   Nothing  = []

instance Arbitrary a => Arbitrary (Maybe a) where
  arbitrary = arbitrary1
  shrink = shrink1

instance Arbitrary2 Either where
  liftArbitrary2 arbA arbB = oneof [liftM Left arbA, liftM Right arbB]

  liftShrink2 shrA _ (Left x)  = [ Left  x' | x' <- shrA x ]
  liftShrink2 _ shrB (Right y) = [ Right y' | y' <- shrB y ]

instance Arbitrary a => Arbitrary1 (Either a) where
  liftArbitrary = liftArbitrary2 arbitrary
  liftShrink = liftShrink2 shrink

instance (Arbitrary a, Arbitrary b) => Arbitrary (Either a b) where
  arbitrary = arbitrary2
  shrink = shrink2

instance Arbitrary1 [] where
  liftArbitrary = listOf
  liftShrink = shrinkList

instance Arbitrary a => Arbitrary [a] where
  arbitrary = arbitrary1
  shrink = shrink1

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
  shrink    = shrinkRealFrac

instance (RealFloat a, Arbitrary a) => Arbitrary (Complex a) where
  arbitrary = liftM2 (:+) arbitrary arbitrary
  shrink (x :+ y) = [ x' :+ y | x' <- shrink x ] ++
                    [ x :+ y' | y' <- shrink y ]

#ifndef NO_FIXED
instance HasResolution a => Arbitrary (Fixed a) where
  arbitrary = arbitrarySizedFractional
  shrink    = shrinkRealFrac
#endif

instance Arbitrary2 (,) where
  liftArbitrary2 = liftM2 (,)
  liftShrink2 shrA shrB (x, y) =
       [ (x', y) | x' <- shrA x ]
    ++ [ (x, y') | y' <- shrB y ]

instance (Arbitrary a) => Arbitrary1 ((,) a) where
  liftArbitrary = liftArbitrary2 arbitrary
  liftShrink = liftShrink2 shrink

instance (Arbitrary a, Arbitrary b) => Arbitrary (a,b) where
  arbitrary = arbitrary2
  shrink = shrink2

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

instance ( Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e
         , Arbitrary f
         )
      => Arbitrary (a,b,c,d,e,f)
 where
  arbitrary = return (,,,,,)
          <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
          <*> arbitrary <*> arbitrary

  shrink (u, v, w, x, y, z) =
    [ (u', v', w', x', y', z')
    | (u', (v', (w', (x', (y', z'))))) <- shrink (u, (v, (w, (x, (y, z))))) ]

instance ( Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e
         , Arbitrary f, Arbitrary g
         )
      => Arbitrary (a,b,c,d,e,f,g)
 where
  arbitrary = return (,,,,,,)
          <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
          <*> arbitrary <*> arbitrary <*> arbitrary

  shrink (t, u, v, w, x, y, z) =
    [ (t', u', v', w', x', y', z')
    | (t', (u', (v', (w', (x', (y', z')))))) <- shrink (t, (u, (v, (w, (x, (y, z)))))) ]

instance ( Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e
         , Arbitrary f, Arbitrary g, Arbitrary h
         )
      => Arbitrary (a,b,c,d,e,f,g,h)
 where
  arbitrary = return (,,,,,,,)
          <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
          <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

  shrink (s, t, u, v, w, x, y, z) =
    [ (s', t', u', v', w', x', y', z')
    | (s', (t', (u', (v', (w', (x', (y', z')))))))
      <- shrink (s, (t, (u, (v, (w, (x, (y, z))))))) ]

instance ( Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e
         , Arbitrary f, Arbitrary g, Arbitrary h, Arbitrary i
         )
      => Arbitrary (a,b,c,d,e,f,g,h,i)
 where
  arbitrary = return (,,,,,,,,)
          <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
          <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
          <*> arbitrary

  shrink (r, s, t, u, v, w, x, y, z) =
    [ (r', s', t', u', v', w', x', y', z')
    | (r', (s', (t', (u', (v', (w', (x', (y', z'))))))))
      <- shrink (r, (s, (t, (u, (v, (w, (x, (y, z)))))))) ]

instance ( Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e
         , Arbitrary f, Arbitrary g, Arbitrary h, Arbitrary i, Arbitrary j
         )
      => Arbitrary (a,b,c,d,e,f,g,h,i,j)
 where
  arbitrary = return (,,,,,,,,,)
          <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
          <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
          <*> arbitrary <*> arbitrary

  shrink (q, r, s, t, u, v, w, x, y, z) =
    [ (q', r', s', t', u', v', w', x', y', z')
    | (q', (r', (s', (t', (u', (v', (w', (x', (y', z')))))))))
      <- shrink (q, (r, (s, (t, (u, (v, (w, (x, (y, z))))))))) ]

-- typical instance for primitive (numerical) types

instance Arbitrary Integer where
  arbitrary = arbitrarySizedIntegral
  shrink    = shrinkIntegral

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
  arbitrary =
    frequency
      [(3, arbitraryASCIIChar),
       (1, arbitraryUnicodeChar)]

  shrink c = filter (<. c) $ nub
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

#ifndef NO_CTYPES
instance Arbitrary CChar where
  arbitrary = CChar <$> arbitrary
  shrink (CChar x) = CChar <$> shrink x

instance Arbitrary CSChar where
  arbitrary = CSChar <$> arbitrary
  shrink (CSChar x) = CSChar <$> shrink x

instance Arbitrary CUChar where
  arbitrary = CUChar <$> arbitrary
  shrink (CUChar x) = CUChar <$> shrink x

instance Arbitrary CShort where
  arbitrary = CShort <$> arbitrary
  shrink (CShort x) = CShort <$> shrink x

instance Arbitrary CUShort where
  arbitrary = CUShort <$> arbitrary
  shrink (CUShort x) = CUShort <$> shrink x

instance Arbitrary CInt where
  arbitrary = CInt <$> arbitrary
  shrink (CInt x) = CInt <$> shrink x

instance Arbitrary CUInt where
  arbitrary = CUInt <$> arbitrary
  shrink (CUInt x) = CUInt <$> shrink x

instance Arbitrary CLong where
  arbitrary = CLong <$> arbitrary
  shrink (CLong x) = CLong <$> shrink x

instance Arbitrary CULong where
  arbitrary = CULong <$> arbitrary
  shrink (CULong x) = CULong <$> shrink x

instance Arbitrary CPtrdiff where
  arbitrary = CPtrdiff <$> arbitrary
  shrink (CPtrdiff x) = CPtrdiff <$> shrink x

instance Arbitrary CSize where
  arbitrary = CSize <$> arbitrary
  shrink (CSize x) = CSize <$> shrink x

instance Arbitrary CWchar where
  arbitrary = CWchar <$> arbitrary
  shrink (CWchar x) = CWchar <$> shrink x

instance Arbitrary CSigAtomic where
  arbitrary = CSigAtomic <$> arbitrary
  shrink (CSigAtomic x) = CSigAtomic <$> shrink x

instance Arbitrary CLLong where
  arbitrary = CLLong <$> arbitrary
  shrink (CLLong x) = CLLong <$> shrink x

instance Arbitrary CULLong where
  arbitrary = CULLong <$> arbitrary
  shrink (CULLong x) = CULLong <$> shrink x

instance Arbitrary CIntPtr where
  arbitrary = CIntPtr <$> arbitrary
  shrink (CIntPtr x) = CIntPtr <$> shrink x

instance Arbitrary CUIntPtr where
  arbitrary = CUIntPtr <$> arbitrary
  shrink (CUIntPtr x) = CUIntPtr <$> shrink x

instance Arbitrary CIntMax where
  arbitrary = CIntMax <$> arbitrary
  shrink (CIntMax x) = CIntMax <$> shrink x

instance Arbitrary CUIntMax where
  arbitrary = CUIntMax <$> arbitrary
  shrink (CUIntMax x) = CUIntMax <$> shrink x

instance Arbitrary CClock where
  arbitrary = CClock <$> arbitrary
  shrink (CClock x) = CClock <$> shrink x

instance Arbitrary CTime where
  arbitrary = CTime <$> arbitrary
  shrink (CTime x) = CTime <$> shrink x

instance Arbitrary CUSeconds where
  arbitrary = CUSeconds <$> arbitrary
  shrink (CUSeconds x) = CUSeconds <$> shrink x

instance Arbitrary CSUSeconds where
  arbitrary = CSUSeconds <$> arbitrary
  shrink (CSUSeconds x) = CSUSeconds <$> shrink x

instance Arbitrary CFloat where
  arbitrary = CFloat <$> arbitrary
  shrink (CFloat x) = CFloat <$> shrink x

instance Arbitrary CDouble where
  arbitrary = CDouble <$> arbitrary
  shrink (CDouble x) = CDouble <$> shrink x
#endif

-- Arbitrary instances for container types
instance (Ord a, Arbitrary a) => Arbitrary (Set.Set a) where
  arbitrary = fmap Set.fromList arbitrary
  shrink = map Set.fromList . shrink . Set.toList
instance (Ord k, Arbitrary k) => Arbitrary1 (Map.Map k) where
  liftArbitrary = fmap Map.fromList . liftArbitrary . liftArbitrary
  liftShrink shr = map Map.fromList . liftShrink (liftShrink shr) . Map.toList
instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (Map.Map k v) where
  arbitrary = arbitrary1
  shrink = shrink1
instance Arbitrary IntSet.IntSet where
  arbitrary = fmap IntSet.fromList arbitrary
  shrink = map IntSet.fromList . shrink . IntSet.toList
instance Arbitrary1 IntMap.IntMap where
  liftArbitrary = fmap IntMap.fromList . liftArbitrary . liftArbitrary
  liftShrink shr = map IntMap.fromList . liftShrink (liftShrink shr) . IntMap.toList
instance Arbitrary a => Arbitrary (IntMap.IntMap a) where
  arbitrary = arbitrary1
  shrink = shrink1
instance Arbitrary1 Sequence.Seq where
  liftArbitrary = fmap Sequence.fromList . liftArbitrary
  liftShrink shr = map Sequence.fromList . liftShrink shr . toList
instance Arbitrary a => Arbitrary (Sequence.Seq a) where
  arbitrary = arbitrary1
  shrink = shrink1

-- Arbitrary instance for Ziplist
instance Arbitrary1 ZipList where
  liftArbitrary = fmap ZipList . liftArbitrary
  liftShrink shr = map ZipList . liftShrink shr . getZipList
instance Arbitrary a => Arbitrary (ZipList a) where
  arbitrary = arbitrary1
  shrink = shrink1

#ifndef NO_TRANSFORMERS
-- Arbitrary instance for transformers' Functors
instance Arbitrary1 Identity where
  liftArbitrary = fmap Identity
  liftShrink shr = map Identity . shr . runIdentity
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = arbitrary1
  shrink = shrink1

instance Arbitrary2 Constant where
  liftArbitrary2 arbA _ = fmap Constant arbA
  liftShrink2 shrA _ = fmap Constant . shrA . getConstant
instance Arbitrary a => Arbitrary1 (Constant a) where
  liftArbitrary = liftArbitrary2 arbitrary
  liftShrink = liftShrink2 shrink
-- Have to be defined explicitly, as Constant is kind polymorphic
instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = fmap Constant arbitrary
  shrink = map Constant . shrink . getConstant

instance (Arbitrary1 f, Arbitrary1 g) => Arbitrary1 (Product f g) where
  liftArbitrary arb = liftM2 Pair (liftArbitrary arb) (liftArbitrary arb)
  liftShrink shr (Pair f g) =
    [ Pair f' g | f' <- liftShrink shr f ] ++
    [ Pair f g' | g' <- liftShrink shr g ]
instance (Arbitrary1 f, Arbitrary1 g, Arbitrary a) => Arbitrary (Product f g a) where
  arbitrary = arbitrary1
  shrink = shrink1

instance (Arbitrary1 f, Arbitrary1 g) => Arbitrary1 (Compose f g) where
  liftArbitrary = fmap Compose . liftArbitrary . liftArbitrary
  liftShrink shr = map Compose . liftShrink (liftShrink shr) . getCompose
instance (Arbitrary1 f, Arbitrary1 g, Arbitrary a) => Arbitrary (Compose f g a) where
  arbitrary = arbitrary1
  shrink = shrink1
#endif

-- Arbitrary instance for Const
instance Arbitrary2 Const where
  liftArbitrary2 arbA _ = fmap Const arbA
  liftShrink2 shrA _ = fmap Const . shrA . getConst
instance Arbitrary a => Arbitrary1 (Const a) where
  liftArbitrary = liftArbitrary2 arbitrary
  liftShrink = liftShrink2 shrink
-- Have to be defined explicitly, as Const is kind polymorphic
instance Arbitrary a => Arbitrary (Const a b) where
  arbitrary = fmap Const arbitrary
  shrink = map Const . shrink . getConst

instance Arbitrary (m a) => Arbitrary (WrappedMonad m a) where
  arbitrary = WrapMonad <$> arbitrary
  shrink (WrapMonad a) = map WrapMonad (shrink a)

instance Arbitrary (a b c) => Arbitrary (WrappedArrow a b c) where
  arbitrary = WrapArrow <$> arbitrary
  shrink (WrapArrow a) = map WrapArrow (shrink a)

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

#if defined(MIN_VERSION_base)
#if MIN_VERSION_base(3,0,0)
instance Arbitrary a => Arbitrary (Monoid.First a) where
  arbitrary = fmap Monoid.First arbitrary
  shrink = map Monoid.First . shrink . Monoid.getFirst

instance Arbitrary a => Arbitrary (Monoid.Last a) where
  arbitrary = fmap Monoid.Last arbitrary
  shrink = map Monoid.Last . shrink . Monoid.getLast
#endif

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

instance Arbitrary QCGen where
  arbitrary = MkGen (\g _ -> g)

instance Arbitrary ExitCode where
  arbitrary = frequency [(1, return ExitSuccess), (3, liftM ExitFailure arbitrary)]

  shrink (ExitFailure x) = ExitSuccess : [ ExitFailure x' | x' <- shrink x ]
  shrink _        = []



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
    do let bits n | n == 0 = 0
                  | otherwise = 1 + bits (n `quot` 2)
           k  = 2^(s*(bits mn `max` bits mx `max` 40) `div` 80)
       n <- choose (toInteger mn `max` (-k), toInteger mx `min` k)
       return (fromInteger n)

-- ** Generators for various kinds of character

-- | Generates any Unicode character (but not a surrogate)
arbitraryUnicodeChar :: Gen Char
arbitraryUnicodeChar =
  arbitraryBoundedEnum `suchThat` (not . isSurrogate)
  where
    isSurrogate c = generalCategory c == Surrogate

-- | Generates a random ASCII character (0-127).
arbitraryASCIIChar :: Gen Char
arbitraryASCIIChar = choose ('\0', '\127')

-- | Generates a printable Unicode character.
arbitraryPrintableChar :: Gen Char
arbitraryPrintableChar = arbitrary `suchThat` isPrint

-- ** Helper functions for implementing shrink

-- | Returns no shrinking alternatives.
shrinkNothing :: a -> [a]
shrinkNothing _ = []

-- | Map a shrink function to another domain. This is handy if your data type
-- has special invariants, but is /almost/ isomorphic to some other type.
--
-- @
-- shrinkOrderedList :: (Ord a, Arbitrary a) => [a] -> [[a]]
-- shrinkOrderedList = shrinkMap sort id
--
-- shrinkSet :: (Ord a, Arbitrary a) => Set a -> Set [a]
-- shrinkSet = shrinkMap fromList toList
-- @
shrinkMap :: Arbitrary a => (a -> b) -> (b -> a) -> b -> [b]
shrinkMap f g = shrinkMapBy f g shrink

-- | Non-overloaded version of `shrinkMap`.
shrinkMapBy :: (a -> b) -> (b -> a) -> (a -> [a]) -> b -> [b]
shrinkMapBy f g shr = map f . shr . g

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

-- | Shrink a fraction.
shrinkRealFrac :: RealFrac a => a -> [a]
shrinkRealFrac x =
  nub $
  [ -x
  | x < 0
  ] ++
  map fromInteger (shrinkIntegral (truncate x))

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

#ifndef NO_TRANSFORMERS
-- CoArbitrary instance for transformers' Functors
instance CoArbitrary a => CoArbitrary (Identity a) where
  coarbitrary = coarbitrary . runIdentity

instance CoArbitrary a => CoArbitrary (Constant a b) where
  coarbitrary = coarbitrary . getConstant
#endif

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

#if defined(MIN_VERSION_base)
#if MIN_VERSION_base(3,0,0)
instance CoArbitrary a => CoArbitrary (Monoid.First a) where
  coarbitrary = coarbitrary . Monoid.getFirst

instance CoArbitrary a => CoArbitrary (Monoid.Last a) where
  coarbitrary = coarbitrary . Monoid.getLast
#endif

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

-- | Generates an infinite list.
infiniteList :: Arbitrary a => Gen [a]
infiniteList = infiniteListOf arbitrary

--------------------------------------------------------------------------
-- the end.
