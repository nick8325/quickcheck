{-# LANGUAGE CPP #-}
module Test.QuickCheck.Random where

#ifdef USE_TF_RANDOM
import System.Random
import System.Random.TF hiding (split, next)

type TheGen = TFGen

newTheGen :: IO TFGen
newTheGen = fmap seedTFGen mkSeedTime
#else
import System.Random

type TheGen = StdGen

newTheGen :: IO StdGen
newTheGen = newStdGen
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
