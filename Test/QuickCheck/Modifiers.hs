{-# LANGUAGE CPP #-}
#ifndef NO_SAFE_HASKELL
{-# LANGUAGE Trustworthy #-}
#endif
#ifndef NO_MULTI_PARAM_TYPE_CLASSES
{-# LANGUAGE MultiParamTypeClasses #-}
#endif
#ifndef NO_NEWTYPE_DERIVING
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
#endif
#ifndef NO_TYPEABLE
{-# LANGUAGE DeriveDataTypeable #-}
#endif
-- | Modifiers for test data.
--
-- These types do things such as restricting the kind of test data that can be generated.
-- They can be pattern-matched on in properties as a stylistic
-- alternative to using explicit quantification.
--
-- __Note__: the contents of this module are re-exported by
-- "Test.QuickCheck". You do not need to import it directly.
--
-- Examples:
--
-- @
-- -- Functions cannot be shown (but see "Test.QuickCheck.Function")
-- prop_TakeDropWhile ('Blind' p) (xs :: ['A']) =
--   takeWhile p xs ++ dropWhile p xs == xs
-- @
--
-- @
-- prop_TakeDrop ('NonNegative' n) (xs :: ['A']) =
--   take n xs ++ drop n xs == xs
-- @
--
-- @
-- -- cycle does not work for empty lists
-- prop_Cycle ('NonNegative' n) ('NonEmpty' (xs :: ['A'])) =
--   take n (cycle xs) == take n (xs ++ cycle xs)
-- @
--
-- @
-- -- Instead of 'forAll' 'orderedList'
-- prop_Sort ('Ordered' (xs :: ['OrdA'])) =
--   sort xs == xs
-- @
module Test.QuickCheck.Modifiers
  (
  -- ** Type-level modifiers for changing generator behavior
    Blind(..)
  , Fixed(..)
  , OrderedList(..)
  , NonEmptyList(..)
  , InfiniteList(..)
  , SortedList(..)
  , Positive(..)
  , Negative(..)
  , NonZero(..)
  , NonNegative(..)
  , NonPositive(..)
  , Large(..)
  , Small(..)
  , Smart(..)
  , Shrink2(..)
#ifndef NO_MULTI_PARAM_TYPE_CLASSES
  , Shrinking(..)
  , ShrinkState(..)
#endif
  , ASCIIString(..)
  , UnicodeString(..)
  , PrintableString(..)
  )
 where

--------------------------------------------------------------------------
-- imports

import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Exception

import Data.List
  ( sort
  )
import Data.Ix (Ix)

#ifndef NO_TYPEABLE
import Data.Typeable (Typeable)
#endif

--------------------------------------------------------------------------
-- | @Blind x@: as x, but x does not have to be in the 'Show' class.
newtype Blind a = Blind {getBlind :: a}
 deriving ( Eq, Ord
#ifndef NO_NEWTYPE_DERIVING
          , Num, Integral, Real, Enum
#endif
#ifndef NO_TYPEABLE
          , Typeable
#endif
          )

instance Functor Blind where
  fmap f (Blind x) = Blind (f x)

instance Show (Blind a) where
  show _ = "(*)"

instance Arbitrary a => Arbitrary (Blind a) where
  arbitrary = Blind `fmap` arbitrary

  shrink (Blind x) = [ Blind x' | x' <- shrink x ]

--------------------------------------------------------------------------
-- | @Fixed x@: as x, but will not be shrunk.
newtype Fixed a = Fixed {getFixed :: a}
 deriving ( Eq, Ord, Show, Read
#ifndef NO_NEWTYPE_DERIVING
          , Num, Integral, Real, Enum
#endif
#ifndef NO_TYPEABLE
          , Typeable
#endif
          )

instance Functor Fixed where
  fmap f (Fixed x) = Fixed (f x)

instance Arbitrary a => Arbitrary (Fixed a) where
  arbitrary = Fixed `fmap` arbitrary

  -- no shrink function

--------------------------------------------------------------------------
-- | @Ordered xs@: guarantees that xs is ordered.
newtype OrderedList a = Ordered {getOrdered :: [a]}
 deriving ( Eq, Ord, Show, Read
#ifndef NO_TYPEABLE
          , Typeable
#endif
          )

instance Functor OrderedList where
  fmap f (Ordered x) = Ordered (map f x)

instance (Ord a, Arbitrary a) => Arbitrary (OrderedList a) where
  arbitrary = Ordered `fmap` orderedList

  shrink (Ordered xs) =
    [ Ordered xs'
    | xs' <- shrink xs
    , sort xs' == xs'
    ]

--------------------------------------------------------------------------
-- | @NonEmpty xs@: guarantees that xs is non-empty.
newtype NonEmptyList a = NonEmpty {getNonEmpty :: [a]}
 deriving ( Eq, Ord, Show, Read
#ifndef NO_TYPEABLE
          , Typeable
#endif
          )

instance Functor NonEmptyList where
  fmap f (NonEmpty x) = NonEmpty (map f x)

instance Arbitrary a => Arbitrary (NonEmptyList a) where
  arbitrary = NonEmpty `fmap` (arbitrary `suchThat` (not . null))

  shrink (NonEmpty xs) =
    [ NonEmpty xs'
    | xs' <- shrink xs
    , not (null xs')
    ]

----------------------------------------------------------------------
-- | @InfiniteList xs _@: guarantees that xs is an infinite list.
-- When a counterexample is found, only prints the prefix of xs
-- that was used by the program.
--
-- Here is a contrived example property:
--
-- > prop_take_10 :: InfiniteList Char -> Bool
-- > prop_take_10 (InfiniteList xs _) =
-- >   or [ x == 'a' | x <- take 10 xs ]
--
-- In the following counterexample, the list must start with @"bbbbbbbbbb"@ but
-- the remaining (infinite) part can contain anything:
--
-- >>> quickCheck prop_take_10
-- *** Failed! Falsified (after 1 test and 14 shrinks):
-- "bbbbbbbbbb" ++ ...
data InfiniteList a =
  InfiniteList {
    getInfiniteList :: [a],
    infiniteListInternalData :: InfiniteListInternalData a }

-- Uses a similar trick to Test.QuickCheck.Function:
-- the Arbitrary instance generates an infinite list, which is
-- reduced to a finite prefix by shrinking. We use discard to
-- check that nothing coming after the finite prefix is used
-- (see infiniteListFromData).
data InfiniteListInternalData a = Infinite [a] | FinitePrefix [a]

infiniteListFromData :: InfiniteListInternalData a -> InfiniteList a
infiniteListFromData info@(Infinite xs) = InfiniteList xs info
infiniteListFromData info@(FinitePrefix xs) =
  InfiniteList (xs ++ discard) info

instance Show a => Show (InfiniteList a) where
  showsPrec _ (InfiniteList _ (Infinite _)) =
    ("<infinite list>" ++)
  showsPrec n (InfiniteList _ (FinitePrefix xs)) =
    (if n > 10 then ('(':) else id) .
    showsPrec 0 xs .
    (" ++ ..." ++) .
    (if n > 10 then (')':) else id)

instance Arbitrary a => Arbitrary (InfiniteList a) where
  arbitrary = fmap infiniteListFromData arbitrary
  shrink (InfiniteList _ info) =
    map infiniteListFromData (shrink info)

instance Arbitrary a => Arbitrary (InfiniteListInternalData a) where
  arbitrary = fmap Infinite infiniteList
  shrink (Infinite xs) =
    [FinitePrefix (take n xs) | n <- map (2^) [0..]]
  shrink (FinitePrefix xs) =
    map FinitePrefix (shrink xs)

--------------------------------------------------------------------------
-- | @Sorted xs@: guarantees that xs is sorted.
newtype SortedList a = Sorted {getSorted :: [a]}
 deriving ( Eq, Ord, Show, Read
#ifndef NO_TYPEABLE
          , Typeable
#endif
          )

instance Functor SortedList where
  fmap f (Sorted x) = Sorted (map f x)

instance (Arbitrary a, Ord a) => Arbitrary (SortedList a) where
  arbitrary = fmap (Sorted . sort) arbitrary

  shrink (Sorted xs) =
    [ Sorted xs'
    | xs' <- map sort (shrink xs)
    ]

--------------------------------------------------------------------------
-- | @Positive x@: guarantees that @x \> 0@.
newtype Positive a = Positive {getPositive :: a}
 deriving ( Eq, Ord, Show, Read
#ifndef NO_NEWTYPE_DERIVING
          , Enum
#endif
#ifndef NO_TYPEABLE
          , Typeable
#endif
          )

instance Functor Positive where
  fmap f (Positive x) = Positive (f x)

instance (Num a, Ord a, Arbitrary a) => Arbitrary (Positive a) where
  arbitrary = fmap Positive (fmap abs arbitrary `suchThat` (> 0))
  shrink (Positive x) = [ Positive x' | x' <- shrink x , x' > 0 ]

--------------------------------------------------------------------------
-- | @Negative x@: guarantees that @x \> 0@.
newtype Negative a = Negative {getNegative :: a}
 deriving ( Eq, Ord, Show, Read
#ifndef NO_NEWTYPE_DERIVING
          , Enum
#endif
#ifndef NO_TYPEABLE
          , Typeable
#endif
          )

instance Functor Negative where
  fmap f (Negative x) = Negative (f x)

instance (Num a, Ord a, Arbitrary a) => Arbitrary (Negative a) where
  arbitrary = fmap Negative (arbitrary `suchThat` (< 0))
  shrink (Negative x) = [ Negative x' | x' <- shrink x , x' < 0 ]

--------------------------------------------------------------------------
-- | @NonZero x@: guarantees that @x \/= 0@.
newtype NonZero a = NonZero {getNonZero :: a}
 deriving ( Eq, Ord, Show, Read
#ifndef NO_NEWTYPE_DERIVING
          , Enum
#endif
#ifndef NO_TYPEABLE
          , Typeable
#endif
          )

instance Functor NonZero where
  fmap f (NonZero x) = NonZero (f x)

instance (Num a, Eq a, Arbitrary a) => Arbitrary (NonZero a) where
  arbitrary = fmap NonZero $ arbitrary `suchThat` (/= 0)

  shrink (NonZero x) = [ NonZero x' | x' <- shrink x, x' /= 0 ]

--------------------------------------------------------------------------
-- | @NonNegative x@: guarantees that @x \>= 0@.
newtype NonNegative a = NonNegative {getNonNegative :: a}
 deriving ( Eq, Ord, Show, Read
#ifndef NO_NEWTYPE_DERIVING
          , Enum
#endif
#ifndef NO_TYPEABLE
          , Typeable
#endif
          )

instance Functor NonNegative where
  fmap f (NonNegative x) = NonNegative (f x)

instance (Num a, Ord a, Arbitrary a) => Arbitrary (NonNegative a) where
  arbitrary = fmap NonNegative (fmap abs arbitrary `suchThat` (>= 0))
  shrink (NonNegative x) = [ NonNegative x' | x' <- shrink x , x' >= 0 ]

--------------------------------------------------------------------------
-- | @NonPositive x@: guarantees that @x \>= 0@.
newtype NonPositive a = NonPositive {getNonPositive :: a}
 deriving ( Eq, Ord, Show, Read
#ifndef NO_NEWTYPE_DERIVING
          , Enum
#endif
#ifndef NO_TYPEABLE
          , Typeable
#endif
          )

instance Functor NonPositive where
  fmap f (NonPositive x) = NonPositive (f x)

instance (Num a, Ord a, Arbitrary a) => Arbitrary (NonPositive a) where
  arbitrary = fmap NonPositive (arbitrary `suchThat` (<= 0))
  shrink (NonPositive x) = [ NonPositive x' | x' <- shrink x , x' <= 0 ]

--------------------------------------------------------------------------
-- | @Large x@: by default, QuickCheck generates 'Int's drawn from a small
-- range. @Large Int@ gives you values drawn from the entire range instead.
newtype Large a = Large {getLarge :: a}
 deriving ( Eq, Ord, Show, Read
#ifndef NO_NEWTYPE_DERIVING
          , Num, Integral, Real, Enum, Ix
#endif
#ifndef NO_TYPEABLE
          , Typeable
#endif
          )

instance Functor Large where
  fmap f (Large x) = Large (f x)

instance (Integral a, Bounded a) => Arbitrary (Large a) where
  arbitrary = fmap Large arbitrarySizedBoundedIntegral
  shrink (Large x) = fmap Large (shrinkIntegral x)

--------------------------------------------------------------------------
-- | @Small x@: generates values of @x@ drawn from a small range.
-- The opposite of 'Large'.
newtype Small a = Small {getSmall :: a}
 deriving ( Eq, Ord, Show, Read
#ifndef NO_NEWTYPE_DERIVING
          , Num, Integral, Real, Enum, Ix
#endif
#ifndef NO_TYPEABLE
          , Typeable
#endif
          )

instance Functor Small where
  fmap f (Small x) = Small (f x)

instance Integral a => Arbitrary (Small a) where
  arbitrary = fmap Small arbitrarySizedIntegral
  shrink (Small x) = map Small (shrinkIntegral x)

--------------------------------------------------------------------------
-- | @Shrink2 x@: allows 2 shrinking steps at the same time when shrinking x
newtype Shrink2 a = Shrink2 {getShrink2 :: a}
 deriving ( Eq, Ord, Show, Read
#ifndef NO_NEWTYPE_DERIVING
          , Num, Integral, Real, Enum
#endif
#ifndef NO_TYPEABLE
          , Typeable
#endif
          )

instance Functor Shrink2 where
  fmap f (Shrink2 x) = Shrink2 (f x)

instance Arbitrary a => Arbitrary (Shrink2 a) where
  arbitrary =
    Shrink2 `fmap` arbitrary

  shrink (Shrink2 x) =
    [ Shrink2 y | y <- shrink_x ] ++
    [ Shrink2 z
    | y <- shrink_x
    , z <- shrink y
    ]
   where
    shrink_x = shrink x

--------------------------------------------------------------------------
-- | @Smart _ x@: tries a different order when shrinking.
data Smart a =
  Smart Int a

instance Functor Smart where
  fmap f (Smart n x) = Smart n (f x)

instance Show a => Show (Smart a) where
  showsPrec n (Smart _ x) = showsPrec n x

instance Arbitrary a => Arbitrary (Smart a) where
  arbitrary =
    do x <- arbitrary
       return (Smart 0 x)

  shrink (Smart i x) = take i' ys `ilv` drop i' ys
   where
    ys = [ Smart j y | (j,y) <- [0..] `zip` shrink x ]
    i' = 0 `max` (i-2)

    []     `ilv` bs     = bs
    as     `ilv` []     = as
    (a:as) `ilv` (b:bs) = a : b : (as `ilv` bs)

{-
  shrink (Smart i x) = part0 ++ part2 ++ part1
   where
    ys = [ Smart i y | (i,y) <- [0..] `zip` shrink x ]
    i' = 0 `max` (i-2)
    k  = i `div` 10

    part0 = take k ys
    part1 = take (i'-k) (drop k ys)
    part2 = drop i' ys
-}

    -- drop a (drop b xs) == drop (a+b) xs           | a,b >= 0
    -- take a (take b xs) == take (a `min` b) xs
    -- take a xs ++ drop a xs == xs

    --    take k ys ++ take (i'-k) (drop k ys) ++ drop i' ys
    -- == take k ys ++ take (i'-k) (drop k ys) ++ drop (i'-k) (drop k ys)
    -- == take k ys ++ take (i'-k) (drop k ys) ++ drop (i'-k) (drop k ys)
    -- == take k ys ++ drop k ys
    -- == ys

#ifndef NO_MULTI_PARAM_TYPE_CLASSES
--------------------------------------------------------------------------
-- | @Shrinking _ x@: allows for maintaining a state during shrinking.
data Shrinking s a =
  Shrinking s a

class ShrinkState s a where
  shrinkInit  :: a -> s
  shrinkState :: a -> s -> [(a,s)]

instance Functor (Shrinking s) where
  fmap f (Shrinking s x) = Shrinking s (f x)

instance Show a => Show (Shrinking s a) where
  showsPrec n (Shrinking _ x) = showsPrec n x

instance (Arbitrary a, ShrinkState s a) => Arbitrary (Shrinking s a) where
  arbitrary =
    do x <- arbitrary
       return (Shrinking (shrinkInit x) x)

  shrink (Shrinking s x) =
    [ Shrinking s' x'
    | (x',s') <- shrinkState x s
    ]

#endif /* NO_MULTI_PARAM_TYPE_CLASSES */

--------------------------------------------------------------------------
-- | @ASCIIString@: generates an ASCII string.
newtype ASCIIString = ASCIIString {getASCIIString :: String}
  deriving ( Eq, Ord, Show, Read
#ifndef NO_TYPEABLE
          , Typeable
#endif
           )

instance Arbitrary ASCIIString where
  arbitrary = ASCIIString `fmap` listOf arbitraryASCIIChar
  shrink (ASCIIString xs) = ASCIIString `fmap` shrink xs

--------------------------------------------------------------------------
-- | @UnicodeString@: generates a unicode String.
-- The string will not contain surrogate pairs.
newtype UnicodeString = UnicodeString {getUnicodeString :: String}
  deriving ( Eq, Ord, Show, Read
#ifndef NO_TYPEABLE
          , Typeable
#endif
           )

instance Arbitrary UnicodeString where
  arbitrary = UnicodeString `fmap` listOf arbitraryUnicodeChar
  shrink (UnicodeString xs) = UnicodeString `fmap` shrink xs

--------------------------------------------------------------------------
-- | @PrintableString@: generates a printable unicode String.
-- The string will not contain surrogate pairs.
newtype PrintableString = PrintableString {getPrintableString :: String}
  deriving ( Eq, Ord, Show, Read
#ifndef NO_TYPEABLE
          , Typeable
#endif
           )

instance Arbitrary PrintableString where
  arbitrary = PrintableString `fmap` listOf arbitraryPrintableChar
  shrink (PrintableString xs) = PrintableString `fmap` shrink xs

-- the end.
