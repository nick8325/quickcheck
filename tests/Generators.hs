{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, Rank2Types, NoMonomorphismRestriction, CPP #-}
import Test.QuickCheck
import Test.QuickCheck.Gen.Unsafe
import Data.List
import Data.Int
import Data.Word
import Data.Version
import Numeric.Natural (Natural)
import System.Exit
import Data.Complex
import Text.ParserCombinators.ReadP (readP_to_S)

newtype Path a = Path [a] deriving (Show, Functor)

instance Arbitrary a => Arbitrary (Path a) where
  arbitrary = do
    x <- arbitrary
    fmap Path (pathFrom 100 x)
    where
      pathFrom n x =
        fmap (x:) $
        case shrink x of
          [] -> return []
          _ | n == 0 -> return []
          ys -> oneof [pathFrom (n-1) y | y <- ys]

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

#if MIN_VERSION_base(4,8,0)
prop_natural :: Path Natural -> Bool
prop_natural = path (\x -> x >= 0 && x <= 100)
#endif

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
reachesBound x = withMaxSuccess 1000 (expectFailure (x < 3 * (maxBound `div` 4)))

prop_reachesBound_Int8 = reachesBound :: Int8 -> Property
prop_reachesBound_Int16 = reachesBound :: Int16 -> Property
prop_reachesBound_Int32 = reachesBound :: Int32 -> Property
prop_reachesBound_Int64 = reachesBound :: Int64 -> Property
prop_reachesBound_Word8 = reachesBound :: Word8 -> Property
prop_reachesBound_Word16 = reachesBound :: Word16 -> Property
prop_reachesBound_Word32 = reachesBound :: Word32 -> Property
prop_reachesBound_Word64 = reachesBound :: Word64 -> Property

-- Shrinking should not loop.
noShrinkingLoop :: (Eq a, Arbitrary a) => Path a -> Bool
noShrinkingLoop (Path (x:xs)) = x `notElem` xs

prop_no_shrinking_loop_Unit = noShrinkingLoop :: Path () -> Bool
prop_no_shrinking_loop_Bool = noShrinkingLoop :: Path Bool -> Bool
prop_no_shrinking_loop_Ordering = noShrinkingLoop :: Path Ordering -> Bool
prop_no_shrinking_loop_Maybe = noShrinkingLoop :: Path (Maybe Int) -> Bool
prop_no_shrinking_loop_Either = noShrinkingLoop :: Path (Either Int Int) -> Bool
prop_no_shrinking_loop_List = noShrinkingLoop :: Path [Int] -> Bool
prop_no_shrinking_loop_Ratio = noShrinkingLoop :: Path Rational -> Bool
prop_no_shrinking_loop_Complex = noShrinkingLoop :: Path (Complex Double) -> Bool
prop_no_shrinking_loop_Fixed = noShrinkingLoop :: Path (Fixed Int) -> Bool
prop_no_shrinking_loop_Pair = noShrinkingLoop :: Path (Int, Int) -> Bool
prop_no_shrinking_loop_Triple = noShrinkingLoop :: Path (Int, Int, Int) -> Bool
prop_no_shrinking_loop_Integer = noShrinkingLoop :: Path Integer -> Bool
prop_no_shrinking_loop_Int = noShrinkingLoop :: Path Int -> Bool
prop_no_shrinking_loop_Int8 = noShrinkingLoop :: Path Int8 -> Bool
prop_no_shrinking_loop_Int16 = noShrinkingLoop :: Path Int16 -> Bool
prop_no_shrinking_loop_Int32 = noShrinkingLoop :: Path Int32 -> Bool
prop_no_shrinking_loop_Int64 = noShrinkingLoop :: Path Int64 -> Bool
prop_no_shrinking_loop_Word = noShrinkingLoop :: Path Word -> Bool
prop_no_shrinking_loop_Word8 = noShrinkingLoop :: Path Word8 -> Bool
prop_no_shrinking_loop_Word16 = noShrinkingLoop :: Path Word16 -> Bool
prop_no_shrinking_loop_Word32 = noShrinkingLoop :: Path Word32 -> Bool
prop_no_shrinking_loop_Word64 = noShrinkingLoop :: Path Word64 -> Bool
prop_no_shrinking_loop_Char = noShrinkingLoop :: Path Char -> Bool
prop_no_shrinking_loop_Float = noShrinkingLoop :: Path Float -> Bool
prop_no_shrinking_loop_Double = noShrinkingLoop :: Path Double -> Bool
prop_no_shrinking_loop_Version = noShrinkingLoop :: Path Version -> Bool
prop_no_shrinking_loop_ExitCode = noShrinkingLoop :: Path ExitCode -> Bool

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
