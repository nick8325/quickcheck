{-# LANGUAGE CPP #-}
module Test.QuickCheck.Random where

import System.Random

newtype QCGen = QCGen StdGen deriving (Read, Show)

instance RandomGen QCGen where
  split (QCGen g) = (QCGen g1, QCGen g2)
    where
      (g1, g2) = split g
  genRange (QCGen g) = genRange g
  next (QCGen g) = (x, QCGen g')
    where
      (x, g') = next g

newQCGen :: IO QCGen
newQCGen = fmap QCGen newStdGen
