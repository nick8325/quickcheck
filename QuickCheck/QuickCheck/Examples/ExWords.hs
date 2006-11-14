module Main where

--------------------------------------------------------------------------
-- imports

import QuickCheck
import Char

{-
data Inv a p = a :? p a

instance Show a => Show (Inv a p) where
  showsPrec n (x :? _) = showsPrec n x

class Invariant p where
  invariant :: p a -> a -> Bool
  theInv    :: p a

data Normal a = Normal

instance Invariant Normal where
  invariant _ c = not (isSpace c && c /= ' ')
  theInv        = Normal

instance (Arbitrary a, Invariant p) => Arbitrary (Inv a p) where
  arbitrary =
    do x <- arbitrary `suchThat` invariant p
       return (x :? p)
   where
    p = theInv

  shrink (x :? p) = [ x' :? p | x' <- shrink x, invariant p x' ]

--------------------------------------------------------------------------
-- example properties

prop_Words s =
  unwords (words s') == s'
 where
  s' = [ c | c :? Normal <- s ]

isStrangeSpace c = isSpace c && c /= ' '
-}

fix f x = if x == x' then x else fix f x' where x' = f x

--------------------------------------------------------------------------
-- the end.


