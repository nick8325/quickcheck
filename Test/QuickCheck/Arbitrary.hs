{-# OPTIONS -fglasgow-exts #-}
module Test.QuickCheck.Arbitrary
  ( 
  -- * Arbitrary and CoArbitrary classes.
    Arbitrary(..)
  , CoArbitrary(..)
  
  -- ** Helper functions for implementing arbitrary
  , arbitrarySizedIntegral   -- :: Num a => Gen a
  , arbitrarySizedFractional -- :: Fractional a => Gen a
  , arbitraryBoundedIntegral -- :: (Bounded a, Integral a) => Gen a
  , arbitraryBoundedRandom   -- :: (Bounded a, Random a) => Gen a
  -- ** Helper functions for implementing shrink
  , shrinkNothing            -- :: a -> [a]
  , shrinkIntegral           -- :: Integral a => a -> [a]
  , shrinkRealFrac           -- :: RealFrac a => a -> [a]
  -- ** Helper functions for implementing coarbitrary
  , (><)
  , coarbitraryIntegral      -- :: Integral a => a -> Gen b -> Gen b
  , coarbitraryReal          -- :: Real a => a -> Gen b -> Gen b
  
  -- ** Generators which use arbitrary
  , vector      -- :: Arbitrary a => Int -> Gen [a]
  , orderedList -- :: (Ord a, Arbitrary a) => Gen [a]

  -- ** Type-level modifiers for changing generator behavior
  , Blind(..)
  , Fixed(..)
  , OrderedList(..)
  , NonEmptyList(..)
  , Positive
  , NonZero(..)
  , NonNegative(..)
  , Smart(..)
  , Shrinking(..)
  , ShrinkState(..)
  )
 where

--------------------------------------------------------------------------
-- imports

import Test.QuickCheck.Gen

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
  )

import Data.Ratio
  ( Ratio
  , (%)
  , numerator
  , denominator
  )

import System.Random
  ( Random
  )

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

--------------------------------------------------------------------------
-- ** class Arbitrary

-- | Random generation and shrinking of values.
class Arbitrary a where
  -- | A generator for values of the given type.
  arbitrary :: Gen a
  arbitrary = error "no default generator"
  
  -- | Produces a (possibly) empty list of all the possible
  -- immediate shrinks of the given value.
  shrink :: a -> [a]
  shrink _ = []

-- instances

instance (CoArbitrary a, Arbitrary b) => Arbitrary (a -> b) where
  arbitrary = promote (`coarbitrary` arbitrary)

instance Arbitrary () where
  arbitrary = return ()

instance Arbitrary Bool where
  arbitrary = choose (False,True)

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

  shrink xs = removeChunks xs
           ++ shrinkOne xs
   where
    removeChunks xs = rem (length xs) xs
     where
      rem 0 _  = []
      rem 1 _  = [[]]
      rem n xs = xs1
               : xs2
               : ( [ xs1' ++ xs2 | xs1' <- rem n1 xs1, not (null xs1') ]
             `ilv` [ xs1 ++ xs2' | xs2' <- rem n2 xs2, not (null xs2') ]
                 )
       where
        n1  = n `div` 2
        xs1 = take n1 xs
        n2  = n - n1
        xs2 = drop n1 xs
    
        []     `ilv` ys     = ys
        xs     `ilv` []     = xs
        (x:xs) `ilv` (y:ys) = x : y : (xs `ilv` ys)
    
    shrinkOne []     = []
    shrinkOne (x:xs) = [ x':xs | x'  <- shrink x ]
                    ++ [ x:xs' | xs' <- shrinkOne xs ] 

{-
  -- "standard" definition for lists:
  shrink []     = []
  shrink (x:xs) = [ xs ]
               ++ [ x:xs' | xs' <- shrink xs ]
               ++ [ x':xs | x'  <- shrink x ]
-}

instance (Integral a, Arbitrary a) => Arbitrary (Ratio a) where
  arbitrary = arbitrarySizedFractional
  shrink    = shrinkRealFrac

instance (Arbitrary a, Arbitrary b)
      => Arbitrary (a,b)
 where
  arbitrary = liftM2 (,) arbitrary arbitrary
  
  shrink (x,y) = [ (x',y) | x' <- shrink x ]
              ++ [ (x,y') | y' <- shrink y ]
              
instance (Arbitrary a, Arbitrary b, Arbitrary c)
      => Arbitrary (a,b,c)
 where
  arbitrary = liftM3 (,,) arbitrary arbitrary arbitrary
  
  shrink (x,y,z) = [ (x',y,z) | x' <- shrink x ]
                ++ [ (x,y',z) | y' <- shrink y ]
                ++ [ (x,y,z') | z' <- shrink z ]
              
instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
      => Arbitrary (a,b,c,d)
 where
  arbitrary = liftM4 (,,,) arbitrary arbitrary arbitrary arbitrary
  
  shrink (w,x,y,z) = [ (w',x,y,z) | w' <- shrink w ]
                  ++ [ (w,x',y,z) | x' <- shrink x ]
                  ++ [ (w,x,y',z) | y' <- shrink y ]
                  ++ [ (w,x,y,z') | z' <- shrink z ]
              
instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e)
      => Arbitrary (a,b,c,d,e)
 where
  arbitrary = liftM5 (,,,,) arbitrary arbitrary arbitrary arbitrary arbitrary
  
  shrink (v,w,x,y,z) = [ (v',w,x,y,z) | v' <- shrink v ]
                    ++ [ (v,w',x,y,z) | w' <- shrink w ]
                    ++ [ (v,w,x',y,z) | x' <- shrink x ]
                    ++ [ (v,w,x,y',z) | y' <- shrink y ]
                    ++ [ (v,w,x,y,z') | z' <- shrink z ]
              
-- typical instance for primitive (numerical) types

instance Arbitrary Integer where
  arbitrary = arbitrarySizedIntegral
  shrink    = shrinkIntegral

instance Arbitrary Int where
  arbitrary = arbitrarySizedIntegral
  shrink    = shrinkIntegral

instance Arbitrary Char where
  arbitrary = chr `fmap` choose (0,255)
  shrink c  = [ c' | c' <- ['a','b','c'], c' < c || not (isLower c) ]

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

-- | Generates an integral number. The number is chosen from the entire
-- range of the type.
arbitraryBoundedIntegral :: (Bounded a, Integral a) => Gen a
arbitraryBoundedIntegral =
  do let mn = minBound
         mx = maxBound `asTypeOf` mn
     n <- choose (toInteger mn, toInteger mx)
     return (fromInteger n `asTypeOf` mn)

-- | Generates an element of a bounded type. The element is
-- chosen from the entire range of the type.
arbitraryBoundedRandom :: (Bounded a, Random a) => Gen a
arbitraryBoundedRandom = choose (minBound,maxBound)

-- ** Helper functions for implementing shrink

-- | Returns no shrinking alternatives. 
shrinkNothing :: a -> [a]
shrinkNothing _ = []

-- | Shrink an integral number.
shrinkIntegral :: Integral a => a -> [a]
shrinkIntegral x = 
  nub $
  [ -x
  | x < 0
  ] ++
  [ x'
  | x' <- takeWhile (<< x) (0:[ x - i | i <- tail (iterate (`quot` 2) x) ])
  ]
 where
  x << y = abs x < abs y

-- | Shrink a fraction.
shrinkRealFrac :: RealFrac a => a -> [a]
shrinkRealFrac x =
  nub $
  [ -x
  | x < 0
  ] ++
  [ x'
  | x' <- [fromInteger (truncate x)]
  , x' << x
  ]
 where
  x << y = abs x < abs y

--------------------------------------------------------------------------
-- ** CoArbitrary

-- | Used for random generation of functions.
class CoArbitrary a where
  -- | Used to generate a function of type @a -> c@. The implementation
  -- should use the first argument to perturb the random generator
  -- given as the second argument. the returned generator 
  -- is then used to generate the function result.
  -- You can often use 'variant' and '><' to implement 
  -- 'coarbitrary'.
  coarbitrary :: a -> Gen c -> Gen c

{-
  -- GHC definition:
  coarbitrary{| Unit |}    Unit      = id
  coarbitrary{| a :*: b |} (x :*: y) = coarbitrary x >< coarbitrary y
  coarbitrary{| a :+: b |} (Inl x)   = variant 0    . coarbitrary x
  coarbitrary{| a :+: b |} (Inr y)   = variant (-1) . coarbitrary y
-}

-- | Combine two generator perturbing functions, for example the 
-- results of calls to 'variant' or 'coarbitrary'.
(><) :: (Gen a -> Gen a) -> (Gen a -> Gen a) -> (Gen a -> Gen a) 
(><) f g gen =
  do n <- arbitrary
     (g . variant (n :: Int) . f) gen 

-- for the sake of non-GHC compilers, I have added definitions
-- for coarbitrary here.

instance (Arbitrary a, CoArbitrary b) => CoArbitrary (a -> b) where
  coarbitrary f gen =
    do xs <- arbitrary
       coarbitrary (map f xs) gen
  
instance CoArbitrary () where
  coarbitrary _ = id

instance CoArbitrary Bool where
  coarbitrary False = variant 0
  coarbitrary True  = variant (-1)

instance CoArbitrary a => CoArbitrary (Maybe a) where
  coarbitrary Nothing  = variant 0
  coarbitrary (Just x) = variant (-1) . coarbitrary x

instance (CoArbitrary a, CoArbitrary b) => CoArbitrary (Either a b) where
  coarbitrary (Left x)  = variant 0    . coarbitrary x
  coarbitrary (Right y) = variant (-1) . coarbitrary y
  
instance CoArbitrary a => CoArbitrary [a] where
  coarbitrary []     = variant 0
  coarbitrary (x:xs) = variant (-1) . coarbitrary (x,xs)

instance (Integral a, CoArbitrary a) => CoArbitrary (Ratio a) where
  coarbitrary r = coarbitrary (numerator r,denominator r)

instance (CoArbitrary a, CoArbitrary b)
      => CoArbitrary (a,b)
 where
  coarbitrary (x,y) = coarbitrary x
                   >< coarbitrary y
              
instance (CoArbitrary a, CoArbitrary b, CoArbitrary c)
      => CoArbitrary (a,b,c)
 where
  coarbitrary (x,y,z) = coarbitrary x
                     >< coarbitrary y
                     >< coarbitrary z
              
instance (CoArbitrary a, CoArbitrary b, CoArbitrary c, CoArbitrary d)
      => CoArbitrary (a,b,c,d)
 where
  coarbitrary (x,y,z,v) = coarbitrary x
                       >< coarbitrary y
                       >< coarbitrary z
                       >< coarbitrary v
              
instance (CoArbitrary a, CoArbitrary b, CoArbitrary c, CoArbitrary d, CoArbitrary e)
      => CoArbitrary (a,b,c,d,e)
 where
  coarbitrary (x,y,z,v,w) = coarbitrary x
                         >< coarbitrary y
                         >< coarbitrary z
                         >< coarbitrary v
                         >< coarbitrary w
              
-- typical instance for primitive (numerical) types

instance CoArbitrary Integer where
  coarbitrary = coarbitraryIntegral

instance CoArbitrary Int where
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

--------------------------------------------------------------------------
-- ** arbitrary generators

-- these are here and not in Gen because of the Arbitrary class constraint

-- | Generates a list of a given length.
vector :: Arbitrary a => Int -> Gen [a]
vector k = vectorOf k arbitrary

-- | Generates an ordered list of a given length.
orderedList :: (Ord a, Arbitrary a) => Gen [a]
orderedList = sort `fmap` arbitrary

--------------------------------------------------------------------------
-- ** arbitrary modifiers

-- | Blind x: as x, but x does not have to be in the 'Show' class.
newtype Blind a = Blind a
 deriving ( Eq, Ord, Num, Enum )

instance Show (Blind a) where
  show _ = "(*)"

instance Arbitrary a => Arbitrary (Blind a) where
  arbitrary = Blind `fmap` arbitrary

  shrink (Blind x) = [ Blind x' | x' <- shrink x ]

-- | Fixed x: as x, but will not be shrunk.
newtype Fixed a = Fixed a
 deriving ( Eq, Ord, Num, Enum, Show, Read )

instance Arbitrary a => Arbitrary (Fixed a) where
  arbitrary = Fixed `fmap` arbitrary
  
  -- no shrink function

-- | Ordered xs: guarantees that xs is ordered.
newtype OrderedList a = Ordered [a]
 deriving ( Eq, Ord, Show, Read )

instance (Ord a, Arbitrary a) => Arbitrary (OrderedList a) where
  arbitrary = Ordered `fmap` orderedList

  shrink (Ordered xs) =
    [ Ordered xs'
    | xs' <- shrink xs
    , sort xs' == xs'
    ]

-- | NonEmpty xs: guarantees that xs is non-empty.
newtype NonEmptyList a = NonEmpty [a]
 deriving ( Eq, Ord, Show, Read )

instance Arbitrary a => Arbitrary (NonEmptyList a) where
  arbitrary = NonEmpty `fmap` (arbitrary `suchThat` (not . null))

  shrink (NonEmpty xs) =
    [ NonEmpty xs'
    | xs' <- shrink xs
    , not (null xs')
    ]

-- | Positive x: guarantees that @x \> 0@.
type Positive a = NonZero (NonNegative a)

-- | NonZero x: guarantees that @x \/= 0@.
newtype NonZero a = NonZero a
 deriving ( Eq, Ord, Num, Integral, Real, Enum, Show, Read )

instance (Num a, Ord a, Arbitrary a) => Arbitrary (NonZero a) where
  arbitrary = fmap NonZero $ arbitrary `suchThat` (/= 0)

  shrink (NonZero x) = [ NonZero x' | x' <- shrink x, x' /= 0 ]

-- | NonNegative x: guarantees that @x \>= 0@.
newtype NonNegative a = NonNegative a
 deriving ( Eq, Ord, Num, Integral, Real, Enum, Show, Read )

instance (Num a, Ord a, Arbitrary a) => Arbitrary (NonNegative a) where
  arbitrary =
    frequency 
      [ (5, (NonNegative . abs) `fmap` arbitrary)
      , (1, return 0)
      ]

  shrink (NonNegative x) =
    [ NonNegative x'
    | x' <- shrink x
    , x' >= 0
    ]

-- | Smart _ x: tries a different order when shrinking.
data Smart a =
  Smart Int a

instance Show a => Show (Smart a) where
  showsPrec n (Smart _ x) = showsPrec n x

instance Arbitrary a => Arbitrary (Smart a) where
  arbitrary =
    do x <- arbitrary
       return (Smart 0 x)

  shrink (Smart i x) = take i' ys `ilv` drop i' ys
   where
    ys = [ Smart i y | (i,y) <- [0..] `zip` shrink x ]
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

-- | Shrinking _ x: allows for maintaining a state during shrinking.
data Shrinking s a =
  Shrinking s a

class ShrinkState s a where
  shrinkInit  :: a -> s
  shrinkState :: a -> s -> [(a,s)]

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

--------------------------------------------------------------------------
-- the end.
