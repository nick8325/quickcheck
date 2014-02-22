-- | Type classes for random generation of values.
{-# LANGUAGE CPP #-}
#ifndef NO_GENERICS
{-# LANGUAGE DefaultSignatures, FlexibleContexts, TypeOperators #-}
#endif
module Test.QuickCheck.Arbitrary
  (
  -- * Arbitrary and CoArbitrary classes
    Arbitrary(..)
  , CoArbitrary(..)

  -- ** Helper functions for implementing arbitrary
  , arbitrarySizedIntegral        -- :: Num a => Gen a
  , arbitraryBoundedIntegral      -- :: (Bounded a, Integral a) => Gen a
  , arbitrarySizedBoundedIntegral -- :: (Bounded a, Integral a) => Gen a
  , arbitrarySizedFractional      -- :: Fractional a => Gen a
  , arbitraryBoundedRandom        -- :: (Bounded a, Random a) => Gen a
  , arbitraryBoundedEnum          -- :: (Bounded a, Enum a) => Gen a
  -- ** Helper functions for implementing shrink
#ifndef NO_GENERICS
  , genericShrink -- :: (Generic a, GenericShrink (Rep a)) => a -> [a]
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

--------------------------------------------------------------------------
-- ** class Arbitrary

-- | Random generation and shrinking of values.
class Arbitrary a where
  -- | A generator for values of the given type.
  arbitrary :: Gen a
  arbitrary = error "no default generator"

  -- | Produces a (possibly) empty list of all the possible
  -- immediate shrinks of the given value. 'genericShrink'
  -- is a reasonable default implementation to try.
  shrink :: a -> [a]
  shrink _ = []

#ifndef NO_GENERICS
-- | Shrink a value generically.
genericShrink :: (Generic a, GenericShrink (Rep a)) => a -> [a]
genericShrink = map to . shrinkRep . from

class GenericShrink f where
  shrinkRep :: f a -> [f a]

instance (GenericShrink f, GenericShrink g) => GenericShrink (f :*: g) where
  shrinkRep (x :*: y) =
    [x' :*: y | x' <- shrinkRep x] ++
    [x :*: y' | y' <- shrinkRep y]

instance (GenericShrink f, GenericShrink g) => GenericShrink (f :+: g) where
  shrinkRep (L1 x) = map L1 (shrinkRep x)
  shrinkRep (R1 x) = map R1 (shrinkRep x)

instance GenericShrink f => GenericShrink (M1 i c f) where
  shrinkRep (M1 x) = map M1 (shrinkRep x)

instance Arbitrary a => GenericShrink (K1 i a) where
  shrinkRep (K1 x) = map K1 (shrink x)

instance GenericShrink U1 where
  shrinkRep U1 = []
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
  arbitrary = sized $ \n ->
    do k <- choose (0,n)
       sequence [ arbitrary | _ <- [1..k] ]

  shrink xs = shrinkList shrink xs

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

instance (Integral a, Arbitrary a) => Arbitrary (Ratio a) where
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

-- ** Helper functions for implementing arbitrary

-- | Generates an integral number. The number can be positive or negative
-- and its maximum absolute value depends on the size parameter.
arbitrarySizedIntegral :: Num a => Gen a
arbitrarySizedIntegral =
  sized $ \n ->
    let n' = toInteger n in
      fmap fromInteger (choose (-n', n'))

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
bounds :: Bounded a => (a -> a -> Gen a) -> Gen a
bounds k = k minBound maxBound

-- | Generates an integral number. The number is chosen uniformly from
-- the entire range of the type. You may want to use
-- 'arbitrarySizedBoundedIntegral' instead.
arbitraryBoundedIntegral :: (Bounded a, Integral a) => Gen a
arbitraryBoundedIntegral =
  bounds $ \mn mx ->
  do n <- choose (toInteger mn, toInteger mx)
     return (fromInteger n)

-- | Generates an element of a bounded type. The element is
-- chosen from the entire range of the type.
arbitraryBoundedRandom :: (Bounded a, Random a) => Gen a
arbitraryBoundedRandom = choose (minBound,maxBound)

-- | Generates an element of a bounded enumeration.
arbitraryBoundedEnum :: (Bounded a, Enum a) => Gen a
arbitraryBoundedEnum =
  bounds $ \mn mx ->
  do n <- choose (fromEnum mn, fromEnum mx)
     return (toEnum n)

-- | Generates an integral number from a bounded domain. The number is
-- chosen from the entire range of the type, but small numbers are
-- generated more often than big numbers. Inspired by demands from
-- Phil Wadler.
arbitrarySizedBoundedIntegral :: (Bounded a, Integral a) => Gen a
arbitrarySizedBoundedIntegral =
  bounds $ \mn mx ->
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

-- | Used for random generation of functions.
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
  coarbitrary (Left x)  = variant 0    . coarbitrary x
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

-- | Generates an ordered list of a given length.
orderedList :: (Ord a, Arbitrary a) => Gen [a]
orderedList = sort `fmap` arbitrary

-- | Generate an infinite list.
infiniteList :: Arbitrary a => Gen [a]
infiniteList = infiniteListOf arbitrary

--------------------------------------------------------------------------
-- the end.
