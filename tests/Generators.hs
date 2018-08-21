{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, Rank2Types, NoMonomorphismRestriction #-}
import Test.QuickCheck
import Test.QuickCheck.Gen.Unsafe
import Data.List
import Data.Int
import Data.Word
import Data.Version (showVersion, parseVersion)
import Text.ParserCombinators.ReadP (readP_to_S)

newtype Path a = Path [a] deriving (Show, Functor)

instance Arbitrary a => Arbitrary (Path a) where
  arbitrary = do
    x <- arbitrary
    fmap Path (pathFrom x)
    where
      pathFrom x = sized $ \n ->
        fmap (x:) $
        oneof $
          [return []] ++
          [resize (n-1) (pathFrom y) | n > 0, y <- shrink x]

  shrink (Path xs) = map Path [ ys | ys <- inits xs, length ys > 0 && length ys < length xs ]

path :: (a -> Bool) -> Path a -> Bool
path p (Path xs) = all p xs

somePath :: (a -> Bool) -> Path a -> Property
somePath p = expectFailure . withMaxSuccess 1000 . path (not . p)

newtype Extremal a = Extremal { getExtremal :: a } deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

instance (Arbitrary a, Bounded a) => Arbitrary (Extremal a) where
  arbitrary =
    fmap Extremal $
    frequency
      [(1, return minBound),
       (1, return maxBound),
       (8, arbitrary)]
  shrink (Extremal x) = map Extremal (shrink x)

smallProp :: Integral a => Path a -> Bool
smallProp = path (\x -> (x >= -100 || -100 `asTypeOf` x >= 0) && x <= 100)

largeProp :: Integral a => Path a -> Property
largeProp = somePath (\x -> x < -1000000 || x > 1000000)

prop_int :: Path Int -> Bool
prop_int = smallProp

prop_int32 :: Path Int32 -> Property
prop_int32 = largeProp

prop_word :: Path Word -> Property
prop_word = largeProp

prop_word32 :: Path Word32 -> Property
prop_word32 = largeProp

prop_integer :: Path Integer -> Bool
prop_integer = smallProp

prop_small :: Path (Small Int) -> Bool
prop_small = smallProp

prop_large :: Path (Large Int) -> Property
prop_large = largeProp

prop_smallWord :: Path (Small Word) -> Bool
prop_smallWord = smallProp

prop_largeWord :: Path (Large Word) -> Property
prop_largeWord = largeProp

data Choice a b = Choice a b deriving Show
instance (Arbitrary a, Arbitrary b) => Arbitrary (Choice a b) where
  arbitrary = do
    Capture eval <- capture
    return (Choice (eval arbitrary) (eval arbitrary))

idemProp :: (Eq a, Arbitrary a, Arbitrary b) => (b -> a) -> Choice a b -> Bool
idemProp f (Choice x y) = x == f y

prop_fixed_length :: Arbitrary a => Path (Fixed a) -> Bool
prop_fixed_length (Path xs) = length xs == 1

prop_fixed_idem = idemProp getFixed
prop_blind_idem = idemProp getBlind

prop_ordered_list = path (\(Ordered xs) -> sort xs == xs)
prop_nonempty_list = path (\(NonEmpty xs) -> not (null xs))

pathInt, somePathInt ::
  (Arbitrary (f (Extremal Int)), Show (f (Extremal Int)),
   Arbitrary (f Integer), Show (f Integer),
   Arbitrary (f (Extremal Int8)), Show (f (Extremal Int8)),
   Arbitrary (f (Extremal Int16)), Show (f (Extremal Int16)),
   Arbitrary (f (Extremal Int32)), Show (f (Extremal Int32)),
   Arbitrary (f (Extremal Int64)), Show (f (Extremal Int64)),
   Arbitrary (f (Extremal Word)), Show (f (Extremal Word)),
   Arbitrary (f (Extremal Word8)), Show (f (Extremal Word8)),
   Arbitrary (f (Extremal Word16)), Show (f (Extremal Word16)),
   Arbitrary (f (Extremal Word32)), Show (f (Extremal Word32)),
   Arbitrary (f (Extremal Word64)), Show (f (Extremal Word64))) =>
  (forall a. f a -> a) -> (forall a. Integral a => a -> Bool) -> Property
pathInt f p =
  conjoin
    [counterexample "Int" (path ((p :: Int -> Bool) . getExtremal . f)),
     counterexample "Integer" (path ((p :: Integer -> Bool) . f)),
     counterexample "Int8" (path ((p :: Int8 -> Bool) . getExtremal . f)),
     counterexample "Int16" (path ((p :: Int16 -> Bool) . getExtremal . f)),
     counterexample "Int32" (path ((p :: Int32 -> Bool) . getExtremal . f)),
     counterexample "Int64" (path ((p :: Int64 -> Bool) . getExtremal . f)),
     counterexample "Word" (path ((p :: Word -> Bool) . getExtremal . f)),
     counterexample "Word8" (path ((p :: Word8 -> Bool) . getExtremal . f)),
     counterexample "Word16" (path ((p :: Word16 -> Bool) . getExtremal . f)),
     counterexample "Word32" (path ((p :: Word32 -> Bool) . getExtremal . f)),
     counterexample "Word64" (path ((p :: Word64 -> Bool) . getExtremal . f))]
somePathInt f p = expectFailure (pathInt f (not . p))

prop_positive = pathInt getPositive (> 0)
prop_positive_bound = somePathInt getPositive (== 1)

prop_nonzero = pathInt getNonZero (/= 0)
prop_nonzero_bound_1 = somePathInt getNonZero (== 1)
prop_nonzero_bound_2 = somePathInt getNonZero (== -1)

prop_nonnegative = pathInt getNonNegative (>= 0)
prop_nonnegative_bound = somePathInt getNonNegative (== 0)

reachesBound :: (Bounded a, Integral a, Arbitrary a) =>
  a -> Property
reachesBound x = expectFailure (x < 3 * (maxBound `div` 4))

prop_reachesBound_Int8 = reachesBound :: Int8 -> Property
prop_reachesBound_Int16 = reachesBound :: Int16 -> Property
prop_reachesBound_Int32 = reachesBound :: Int32 -> Property
prop_reachesBound_Int64 = reachesBound :: Int64 -> Property
prop_reachesBound_Word8 = reachesBound :: Word8 -> Property
prop_reachesBound_Word16 = reachesBound :: Word16 -> Property
prop_reachesBound_Word32 = reachesBound :: Word32 -> Property
prop_reachesBound_Word64 = reachesBound :: Word64 -> Property

-- Bad shrink: infinite list
--
-- remove unexpectedFailure in prop_B1, shrinking should not loop forever.
data B1 = B1 Int deriving (Eq, Show)

instance Arbitrary B1 where
    arbitrary = fmap B1 arbitrary
    shrink x = x : shrink x

prop_B1 :: B1 -> Property
prop_B1 (B1 n) = expectFailure $ n === n + 1

return []
main = do True <- $forAllProperties (quickCheckWithResult stdArgs { maxShrinks = 10000 }); return ()
