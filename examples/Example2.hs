module Main where

--------------------------------------------------------------------------
-- imports

-- QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Property
import Test.QuickCheck.Test
import Test.QuickCheck.Monadic

-- other
import Control.Monad.ST
import Data.STRef
import Data.Char

--------------------------------------------------------------------------
-- example

prop_WriteThenRead =
  monadicST (
    do ref <- run (newSTRef undefined)
       x <- pick (arbitrary :: Gen Int)
       run (writeSTRef ref x)
       y <- run (readSTRef ref)
       assert (x == y)
  )

newtype PrintableChar = Printable{ unPrintable :: Char }

instance Arbitrary PrintableChar where
  arbitrary = (Printable . chr) `fmap` choose (32,127)

{-
propIO_WriteFileThenRead =
  monadic $
    do s <- map unPrintable `fmap` pick (arbitrary :: Gen [PrintableChar])
       run (writeFile "apa" s)
       s' <- run (readFile "apa")
       assert (s == s')
-}

main =
  do quickCheck prop_WriteThenRead
     --quickCheckIO propIO_WriteFileThenRead

--------------------------------------------------------------------------
-- the end.
