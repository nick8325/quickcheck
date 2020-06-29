{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
-- | A wrapper around the system random number generator. Internal QuickCheck module.
{-# LANGUAGE CPP #-}
#ifndef NO_SAFE_HASKELL
{-# LANGUAGE Trustworthy #-}
#endif
module Test.QuickCheck.Random where

import System.Random
#ifndef NO_SPLITMIX
import System.Random.SplitMix
#endif
import Data.Bits

-- | The "standard" QuickCheck random number generator.
-- A wrapper around either 'SMGen' on GHC, or 'StdGen'
-- on other Haskell systems.
#ifdef NO_SPLITMIX
newtype QCGen = QCGen StdGen
#else
newtype QCGen = QCGen SMGen
#endif

instance Show QCGen where
  showsPrec n (QCGen g) s = showsPrec n g s
instance Read QCGen where
  readsPrec n xs = [(QCGen g, ys) | (g, ys) <- readsPrec n xs]

instance RandomGen QCGen where
#ifdef NO_SPLITMIX
  split (QCGen g) =
    case split g of
      (g1, g2) -> (QCGen g1, QCGen g2)
  genRange (QCGen g) = genRange g
  next = wrapQCGen next
#else
  split (QCGen g) =
    case splitSMGen g of
      (g1, g2) -> (QCGen g1, QCGen g2)
  genRange _ = (minBound, maxBound)
  next = wrapQCGen nextInt

#ifndef OLD_RANDOM
  genWord8 = wrapQCGen genWord8
  genWord16 = wrapQCGen genWord16
  genWord32 = wrapQCGen genWord32
  genWord64 = wrapQCGen genWord64
  genWord32R r = wrapQCGen (genWord32R r)
  genWord64R r = wrapQCGen (genWord64R r)
  genShortByteString n = wrapQCGen (genShortByteString n)
#endif
#endif

{-# INLINE wrapQCGen #-}
#ifdef NO_SPLITMIX
wrapQCGen :: (StdGen -> (a, StdGen)) -> (QCGen -> (a, QCGen))
#else
wrapQCGen :: (SMGen -> (a, SMGen)) -> (QCGen -> (a, QCGen))
#endif
wrapQCGen f (QCGen g) =
  case f g of
    (x, g') -> (x, QCGen g')

newQCGen :: IO QCGen
#ifdef NO_SPLITMIX
newQCGen = fmap QCGen newStdGen
#else
newQCGen = fmap QCGen newSMGen
#endif

mkQCGen :: Int -> QCGen
#ifdef NO_SPLITMIX
mkQCGen n = QCGen (mkStdGen n)
#else
mkQCGen n = QCGen (mkSMGen (fromIntegral n))
#endif

-- Parameterised in order to make this code testable.
class Splittable a where
  left, right :: a -> a

instance Splittable QCGen where
  left = fst . split
  right = snd . split

-- The logic behind 'variant'. Given a random number seed, and an integer, uses
-- splitting to transform the seed according to the integer. We use a
-- prefix-free code so that calls to integerVariant n g for different values of
-- n are guaranteed to return independent seeds.
{-# INLINE integerVariant #-}
integerVariant :: Splittable a => Integer -> a -> a
integerVariant n g
  -- Use one bit to encode the sign, then use Elias gamma coding
  -- (https://en.wikipedia.org/wiki/Elias_gamma_coding) to do the rest.
  -- Actually, the first bit encodes whether n >= 1 or not;
  -- this has the advantage that both 0 and 1 get short codes.
  | n >= 1 = gamma n $! left g
  | otherwise = gamma (1-n) $! right g
  where
    gamma n =
      encode k . zeroes k
      where
        k = ilog2 n

        encode (-1) g = g
        encode k g
          | testBit n k =
            encode (k-1) $! right g
          | otherwise =
            encode (k-1) $! left g

        zeroes 0 g = g
        zeroes k g = zeroes (k-1) $! left g

    ilog2 1 = 0
    ilog2 n = 1 + ilog2 (n `div` 2)
