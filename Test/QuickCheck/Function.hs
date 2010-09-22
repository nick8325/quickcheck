{-# LANGUAGE TypeOperators, GADTs #-}
module Test.QuickCheck.Function
  ( Fun(..)
  , apply
  , (:->)
  , FunArbitrary(..)
  , funArbitraryMap
  , funArbitraryShow
  )
 where

--------------------------------------------------------------------------
-- imports

import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Property
import Test.QuickCheck.Poly
import Test.QuickCheck.Modifiers

import Data.Char
import Data.Word

--------------------------------------------------------------------------
-- concrete functions

-- the type of possibly partial concrete functions
data a :-> c where
  Pair  :: (a :-> (b :-> c)) -> ((a,b) :-> c)
  (:+:) :: (a :-> c) -> (b :-> c) -> (Either a b :-> c)
  Unit  :: c -> (() :-> c)
  Nil   :: a :-> c
  Table :: Eq a => [(a,c)] -> (a :-> c)
  Map   :: (a -> b) -> (b -> a) -> (b :-> c) -> (a :-> c)

instance Functor ((:->) a) where
  fmap f (Pair p)    = Pair (fmap (fmap f) p)
  fmap f (p:+:q)     = fmap f p :+: fmap f q
  fmap f (Unit c)    = Unit (f c)
  fmap f Nil         = Nil
  fmap f (Table xys) = Table [ (x,f y) | (x,y) <- xys ]
  fmap f (Map g h p) = Map g h (fmap f p)

instance (Show a, Show b) => Show (a:->b) where
  -- only use this on finite functions
  show p =
    "{" ++ (case table p of
             []        -> ""
             (_,c):xcs -> concat [ show x ++ "->" ++ show c ++ ","
                                 | (x,c) <- xcs
                                 ]
                       ++ "_->" ++ show c)
        ++ "}"
   where
    xcs = table p

-- turning a concrete function into an abstract function (with a default result)
abstract :: (a :-> c) -> c -> (a -> c)
abstract (Pair p)    d (x,y) = abstract (fmap (\q -> abstract q d y) p) d x
abstract (p :+: q)   d exy   = either (abstract p d) (abstract q d) exy
abstract (Unit c)    _ _     = c
abstract Nil         d _     = d
abstract (Table xys) d x     = head ([y | (x',y) <- xys, x == x'] ++ [d])
abstract (Map g _ p) d x     = abstract p d (g x)

-- generating a table from a concrete function
table :: (a :-> c) -> [(a,c)]
table (Pair p)    = [ ((x,y),c) | (x,q) <- table p, (y,c) <- table q ]
table (p :+: q)   = [ (Left x, c) | (x,c) <- table p ]
                 ++ [ (Right y,c) | (y,c) <- table q ]
table (Unit c)    = [ ((), c) ]
table Nil         = []
table (Table xys) = xys
table (Map _ h p) = [ (h x, c) | (x,c) <- table p ]

--------------------------------------------------------------------------
-- FunArbitrary

class FunArbitrary a where
  funArbitrary :: Arbitrary c => Gen (a :-> c)

instance (FunArbitrary a, Arbitrary c) => Arbitrary (a :-> c) where
  arbitrary = funArbitrary
  shrink    = shrinkFun shrink

-- basic instances: pairs, sums, units

instance (FunArbitrary a, FunArbitrary b) => FunArbitrary (a,b) where
  funArbitrary =
    do p <- funArbitrary
       return (Pair p)

instance (FunArbitrary a, FunArbitrary b) => FunArbitrary (Either a b) where
  funArbitrary =
    do p <- funArbitrary
       q <- funArbitrary
       return (p :+: q)

instance FunArbitrary () where
  funArbitrary =
    do c <- arbitrary
       return (Unit c)

instance FunArbitrary Word8 where
  funArbitrary =
    do xys <- sequence [ do y <- arbitrary
                            return (x,y)
                       | x <- [0..255]
                       ]
       return (Table xys)

-- other instances (using Map)

funArbitraryMap :: (FunArbitrary a, Arbitrary c) => (b -> a) -> (a -> b) -> Gen (b :-> c)
funArbitraryMap g h =
  do p <- funArbitrary
     return (Map g h p)

funArbitraryShow :: (Show a, Read a, Arbitrary c) => Gen (a :-> c)
funArbitraryShow = funArbitraryMap show read

instance FunArbitrary a => FunArbitrary [a] where
  funArbitrary = funArbitraryMap g h
   where
    g []     = Left ()
    g (x:xs) = Right (x,xs)

    h (Left _)       = []
    h (Right (x,xs)) = x:xs

instance FunArbitrary a => FunArbitrary (Maybe a) where
  funArbitrary = funArbitraryMap g h
   where
    g Nothing  = Left ()
    g (Just x) = Right x

    h (Left _)  = Nothing
    h (Right x) = Just x

instance FunArbitrary Bool where
  funArbitrary = funArbitraryMap g h
   where
    g False = Left ()
    g True  = Right ()
    
    h (Left _)  = False
    h (Right _) = True

instance FunArbitrary Integer where
  funArbitrary = funArbitraryMap gInteger hInteger
   where
    gInteger n | n < 0     = Left (gNatural (abs n - 1))
               | otherwise = Right (gNatural n)
    
    hInteger (Left ws)  = -(hNatural ws + 1)
    hInteger (Right ws) = hNatural ws
    
    gNatural 0 = []
    gNatural n = (fromIntegral (n `mod` 256) :: Word8) : gNatural (n `div` 256)
    
    hNatural []     = 0
    hNatural (w:ws) = fromIntegral w + 256 * hNatural ws

instance FunArbitrary Int where
  funArbitrary = funArbitraryMap fromIntegral fromInteger

instance FunArbitrary Char where
  funArbitrary = funArbitraryMap ord' chr'
   where
    ord' c = fromIntegral (ord c) :: Word8
    chr' n = chr (fromIntegral n)

-- poly instances

instance FunArbitrary A where
  funArbitrary = funArbitraryMap unA A

instance FunArbitrary B where
  funArbitrary = funArbitraryMap unB B

instance FunArbitrary C where
  funArbitrary = funArbitraryMap unC C

instance FunArbitrary OrdA where
  funArbitrary = funArbitraryMap unOrdA OrdA

instance FunArbitrary OrdB where
  funArbitrary = funArbitraryMap unOrdB OrdB

instance FunArbitrary OrdC where
  funArbitrary = funArbitraryMap unOrdC OrdC

--------------------------------------------------------------------------
-- shrinking

shrinkFun :: (c -> [c]) -> (a :-> c) -> [a :-> c]
shrinkFun shr (Pair p) =
  [ pair p' | p' <- shrinkFun (\q -> shrinkFun shr q) p ]
 where
  pair Nil = Nil
  pair p   = Pair p

shrinkFun shr (p :+: q) =
  [ p .+. Nil | not (isNil q) ] ++
  [ Nil .+. q | not (isNil p) ] ++
  [ p' .+. q  | p' <- shrinkFun shr p ] ++
  [ p  .+. q' | q' <- shrinkFun shr q ]
 where
  isNil :: (a :-> b) -> Bool
  isNil Nil = True
  isNil _   = False
 
  Nil .+. Nil = Nil
  p   .+. q   = p :+: q

shrinkFun shr (Unit c) =
  [ Nil ] ++
  [ Unit c' | c' <- shr c ]

shrinkFun shr (Table xys) =
  [ table xys' | xys' <- shrinkList shrXy xys ]
 where
  shrXy (x,y) = [(x,y') | y' <- shr y]
  
  table []  = Nil
  table xys = Table xys

shrinkFun shr Nil =
  []

shrinkFun shr (Map g h p) =
  [ mapp g h p' | p' <- shrinkFun shr p ]
 where
  mapp g h Nil = Nil
  mapp g h p   = Map g h p

--------------------------------------------------------------------------
-- the Fun modifier

data Fun a b = Fun (a :-> b) (a -> b)

fun :: (a :-> b) -> Fun a b
fun p = Fun p (abstract p (snd (head (table p))))

apply :: Fun a b -> (a -> b)
apply (Fun _ f) = f

instance (Show a, Show b) => Show (Fun a b) where
  show (Fun p _) = show p

instance (FunArbitrary a, Arbitrary b) => Arbitrary (Fun a b) where
  arbitrary = fun `fmap` arbitrary

  shrink (Fun p _) =
    [ fun p' | p' <- shrink p, _:_ <- [table p'] ]

--------------------------------------------------------------------------
-- the end.
