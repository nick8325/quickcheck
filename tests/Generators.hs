{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, Rank2Types, NoMonomorphismRestriction #-}
import Test.QuickCheck
import Test.QuickCheck.Gen.Unsafe
import Data.List
import Data.Int
import Data.Word

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
          [resize (n-1) (pathFrom y) | y <- shrink x]

  shrink (Path xs) = map Path [ ys | ys <- inits xs, length ys > 0 && length ys < length xs ]

path :: (a -> Bool) -> Path a -> Bool
path p (Path xs) = all p xs

somePath :: (a -> Bool) -> Path a -> Property
somePath p = expectFailure . path (not . p)

smallProp :: Integral a => Path a -> Bool
smallProp = path (\x -> x >= -100 && x <= 100)

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
  (Arbitrary (f Int), Show (f Int),
   Arbitrary (f Integer), Show (f Integer),
   Arbitrary (f Int8), Show (f Int8),
   Arbitrary (f Int16), Show (f Int16),
   Arbitrary (f Int32), Show (f Int32),
   Arbitrary (f Int64), Show (f Int64),
   Arbitrary (f Word), Show (f Word),
   Arbitrary (f Word8), Show (f Word8),
   Arbitrary (f Word16), Show (f Word16),
   Arbitrary (f Word32), Show (f Word32),
   Arbitrary (f Word64), Show (f Word64)) =>
  (forall a. f a -> a) -> (forall a. Integral a => a -> Bool) -> Property
pathInt f p =
  conjoin
    [counterexample "Int" (path ((p :: Int -> Bool) . f)),
     counterexample "Integer" (path ((p :: Integer -> Bool) . f)),
     counterexample "Int8" (path ((p :: Int8 -> Bool) . f)),
     counterexample "Int16" (path ((p :: Int16 -> Bool) . f)),
     counterexample "Int32" (path ((p :: Int32 -> Bool) . f)),
     counterexample "Int64" (path ((p :: Int64 -> Bool) . f)),
     counterexample "Word" (path ((p :: Word -> Bool) . f)),
     counterexample "Word8" (path ((p :: Word8 -> Bool) . f)),
     counterexample "Word16" (path ((p :: Word16 -> Bool) . f)),
     counterexample "Word32" (path ((p :: Word32 -> Bool) . f)),
     counterexample "Word64" (path ((p :: Word64 -> Bool) . f))]
somePathInt f p = expectFailure (pathInt f (not . p))

prop_positive = pathInt getPositive (> 0)
prop_positive_bound = somePathInt getPositive (== 1)

prop_nonzero = pathInt getNonZero (/= 0)
prop_nonzero_bound_1 = somePathInt getNonZero (== 1)
prop_nonzero_bound_2 = somePathInt getNonZero (== -1)

prop_nonnegative = pathInt getNonNegative (>= 0)
prop_nonnegative_bound = somePathInt getNonNegative (== 0)

main = $quickCheckAll
