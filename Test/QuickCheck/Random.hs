{-# LANGUAGE CPP #-}
module Test.QuickCheck.Random where

#ifdef USE_TF_RANDOM
import System.Random
import System.Random.TF hiding (split, next)
import Data.Word
import Data.Bits

type TheGen = TFGen

newTheGen :: IO TFGen
newTheGen = fmap seedTFGen mkSeedTime

chip :: Bool -> Word32 -> TFGen -> TFGen
chip done n g = splitn g 16 (if done then m .|. 0x8000 else m)
  where
    m = n .&. 0x7fff

chop :: Integer -> Integer
chop n = n `shiftR` 15

stop :: Integral a => a -> Bool
stop n = n <= 0x7fff

#else
import System.Random

type TheGen = StdGen

newTheGen :: IO StdGen
newTheGen = newStdGen

chip :: Bool -> Int -> StdGen -> StdGen
chip finished n = boolVariant finished . boolVariant (even n)

chop :: Integer -> Integer
chop n = n `div` 2

stop :: Integral a => a -> Bool
stop n = n <= 1
#endif

newtype QCGen = QCGen TheGen deriving (Read, Show)

instance RandomGen QCGen where
  split (QCGen g) = (QCGen g1, QCGen g2)
    where
      (g1, g2) = split g
  genRange (QCGen g) = genRange g
  next (QCGen g) = (x, QCGen g')
    where
      (x, g') = next g

newQCGen :: IO QCGen
newQCGen = fmap QCGen newTheGen

bigNatVariant :: Integer -> TheGen -> TheGen
bigNatVariant n g
  | g `seq` stop n = chip True (fromInteger n) g
  | otherwise      = bigNatVariant (chop n) $! chip False (fromInteger n) g

{-# INLINE natVariant #-}
natVariant :: Integral a => a -> TheGen -> TheGen
natVariant n g
  | g `seq` stop n = chip True (fromIntegral n) g
  | otherwise      = bigNatVariant (toInteger n) g

{-# INLINE variantTheGen #-}
variantTheGen :: Integral a => a -> TheGen -> TheGen
variantTheGen n g
  | n >= 0    = natVariant n (boolVariant False g)
  | n == -1   = natVariant 0 (boolVariant True g)
  | otherwise = bigNatVariant (negate (toInteger n)-1) (boolVariant True g)

boolVariant :: Bool -> TheGen -> TheGen
boolVariant False = fst . split
boolVariant True = snd . split

variantQCGen :: Integral a => a -> QCGen -> QCGen
variantQCGen n (QCGen g) = QCGen (variantTheGen n g)
