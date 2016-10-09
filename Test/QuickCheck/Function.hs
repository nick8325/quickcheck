{-# LANGUAGE TypeOperators, GADTs, CPP #-}
#ifndef NO_SAFE_HASKELL
{-# LANGUAGE Safe #-}
#endif
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
#endif

#ifndef NO_GENERICS
{-# LANGUAGE DefaultSignatures, FlexibleContexts #-}
#endif

#ifndef NO_POLYKINDS
{-# LANGUAGE PolyKinds #-}
#endif

-- | Generation of random shrinkable, showable functions.
-- See the paper \"Shrinking and showing functions\" by Koen Claessen.
--
-- Example of use:
--
-- >>> :{
-- >>> let prop :: Fun String Integer -> Bool
-- >>>     prop (Fun _ f) = f "monkey" == f "banana" || f "banana" == f "elephant"
-- >>> :}
-- >>> quickCheck prop
-- *** Failed! Falsifiable (after 3 tests and 134 shrinks):
-- {"elephant"->1, "monkey"->1, _->0}
--
-- To generate random values of type @'Fun' a b@,
-- you must have an instance @'Function' a@.
-- If your type has a 'Show' instance, you can use 'functionShow' to write the instance; otherwise,
-- use 'functionMap' to give a bijection between your type and a type that is already an instance of 'Function'.
-- See the @'Function' [a]@ instance for an example of the latter.
module Test.QuickCheck.Function
  ( Fun(..)
  , Fun2
  , Fun3
  , appFun
  , apply
  , appFun2
  , appFun3
  , (:->)
  , Function(..)
  , functionMap
  , functionShow
  , functionIntegral
  , functionRealFrac
  , functionBoundedEnum
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 708
  , pattern Fn
  , pattern Fn2
  , pattern Fn3
#endif
  )
 where

--------------------------------------------------------------------------
-- imports

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Poly

import Data.Char
import Data.Word
import Data.List( intersperse )
import Data.Maybe( fromJust )
import Data.Ratio
import Control.Arrow( (&&&) )
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Sequence
import Data.Int
import Data.Word
import Data.Complex
import Data.Foldable(toList)

#ifndef NO_FIXED
import Data.Fixed
#endif

#ifndef NO_NATURALS
import Numeric.Natural
#endif

#ifndef NO_PROXY
import Data.Proxy (Proxy (..))
#endif

#ifndef NO_NONEMPTY
import Data.List.NonEmpty(NonEmpty(..))
#endif

#ifndef NO_GENERICS
import GHC.Generics hiding (C)
#endif

--------------------------------------------------------------------------
-- concrete functions

-- | The type of possibly partial concrete functions
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
  show p = showFunction p Nothing

-- only use this on finite functions
showFunction :: (Show a, Show b) => (a :-> b) -> Maybe b -> String
showFunction p md =
  "{" ++ concat (intersperse ", " ( [ show x ++ "->" ++ show c
                                    | (x,c) <- table p
                                    ]
                                 ++ [ "_->" ++ show d
                                    | Just d <- [md]
                                    ] )) ++ "}"

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
-- Function

class Function a where
  function :: (a->b) -> (a:->b)
#ifndef NO_GENERICS
  default function :: (Generic a, GFunction (Rep a)) => (a->b) -> (a:->b)
  function = genericFunction
#endif

-- basic instances

-- | Provides a 'Function' instance for types with 'Bounded' and 'Enum'.
-- Use only for small types (i.e. not integers): creates
-- the list @['minBound'..'maxBound']@!
functionBoundedEnum :: (Eq a, Bounded a, Enum a) => (a->b) -> (a:->b)
functionBoundedEnum f = Table [(x,f x) | x <- [minBound..maxBound]]

-- | Provides a 'Function' instance for types with 'RealFrac'.
functionRealFrac :: RealFrac a => (a->b) -> (a:->b)
functionRealFrac = functionMap toRational fromRational

-- | Provides a 'Function' instance for types with 'Integral'.
functionIntegral :: Integral a => (a->b) -> (a:->b)
functionIntegral = functionMap fromIntegral fromInteger

-- | Provides a 'Function' instance for types with 'Show' and 'Read'.
functionShow :: (Show a, Read a) => (a->c) -> (a:->c)
functionShow f = functionMap show read f

-- | The basic building block for 'Function' instances.
-- Provides a 'Function' instance by mapping to and from a type that
-- already has a 'Function' instance.
functionMap :: Function b => (a->b) -> (b->a) -> (a->c) -> (a:->c)
functionMap = functionMapWith function

functionMapWith :: ((b->c) -> (b:->c)) -> (a->b) -> (b->a) -> (a->c) -> (a:->c)
functionMapWith function g h f = Map g h (function (\b -> f (h b)))

instance Function () where
  function f = Unit (f ())

instance (Function a, Function b) => Function (a,b) where
  function = functionPairWith function function

functionPairWith :: ((a->b->c) -> (a:->(b->c))) -> ((b->c) -> (b:->c)) -> ((a,b)->c) -> ((a,b):->c)
functionPairWith func1 func2 f = Pair (func2 `fmap` func1 (curry f))

instance (Function a, Function b) => Function (Either a b) where
  function = functionEitherWith function function

functionEitherWith :: ((a->c) -> (a:->c)) -> ((b->c) -> (b:->c)) -> (Either a b->c) -> (Either a b:->c)
functionEitherWith func1 func2 f = func1 (f . Left) :+: func2 (f . Right)

-- tuple convenience instances

instance (Function a, Function b, Function c) => Function (a,b,c) where
  function = functionMap (\(a,b,c) -> (a,(b,c))) (\(a,(b,c)) -> (a,b,c))

instance (Function a, Function b, Function c, Function d) => Function (a,b,c,d) where
  function = functionMap (\(a,b,c,d) -> (a,(b,c,d))) (\(a,(b,c,d)) -> (a,b,c,d))

instance (Function a, Function b, Function c, Function d, Function e) => Function (a,b,c,d,e) where
  function = functionMap (\(a,b,c,d,e) -> (a,(b,c,d,e))) (\(a,(b,c,d,e)) -> (a,b,c,d,e))

instance (Function a, Function b, Function c, Function d, Function e, Function f) => Function (a,b,c,d,e,f) where
  function = functionMap (\(a,b,c,d,e,f) -> (a,(b,c,d,e,f))) (\(a,(b,c,d,e,f)) -> (a,b,c,d,e,f))

instance (Function a, Function b, Function c, Function d, Function e, Function f, Function g) => Function (a,b,c,d,e,f,g) where
  function = functionMap (\(a,b,c,d,e,f,g) -> (a,(b,c,d,e,f,g))) (\(a,(b,c,d,e,f,g)) -> (a,b,c,d,e,f,g))

-- other instances

instance Function a => Function [a] where
  function = functionMap g h
   where
    g []     = Left ()
    g (x:xs) = Right (x,xs)

    h (Left _)       = []
    h (Right (x,xs)) = x:xs

instance Function a => Function (Maybe a) where
  function = functionMap g h
   where
    g Nothing  = Left ()
    g (Just x) = Right x

    h (Left _)  = Nothing
    h (Right x) = Just x

instance Function Bool where
  function = functionMap g h
   where
    g False = Left ()
    g True  = Right ()

    h (Left _)  = False
    h (Right _) = True

instance Function Integer where
  function = functionMap gInteger hInteger
   where
    gInteger n | n < 0     = Left (gNatural (abs n - 1))
               | otherwise = Right (gNatural n)

    hInteger (Left ws)  = -(hNatural ws + 1)
    hInteger (Right ws) = hNatural ws

    gNatural 0 = []
    gNatural n = (fromIntegral (n `mod` 256) :: Word8) : gNatural (n `div` 256)

    hNatural []     = 0
    hNatural (w:ws) = fromIntegral w + 256 * hNatural ws

instance Function Int where
  function = functionIntegral

instance Function Char where
  function = functionMap ord chr

instance Function Float where
  function = functionRealFrac

instance Function Double where
  function = functionRealFrac

-- instances for assorted types in the base package

instance Function Ordering where
  function = functionMap g h
    where
      g LT = Left False
      g EQ = Left True
      g GT = Right ()

      h (Left False) = LT
      h (Left True)  = EQ
      h (Right _)    = GT

#ifndef NO_NONEMPTY
instance Function a => Function (NonEmpty a) where
  function = functionMap g h
   where
     g (x :| xs) = (x,   xs)
     h (x,   xs) =  x :| xs
#endif

instance (Integral a, Function a) => Function (Ratio a) where
  function = functionMap g h
   where
     g r = (numerator r, denominator r)
     h (n, d) = n % d

#ifndef NO_FIXED
instance HasResolution a => Function (Fixed a) where
  function = functionRealFrac
#endif

instance (RealFloat a, Function a) => Function (Complex a) where
  function = functionMap g h
   where
     g (x :+ y) = (x,   y)
     h (x,   y) =  x :+ y

instance (Ord a, Function a) => Function (Set.Set a) where
  function = functionMap Set.toList Set.fromList

instance (Ord a, Function a, Function b) => Function (Map.Map a b) where
  function = functionMap Map.toList Map.fromList

instance Function IntSet.IntSet where
  function = functionMap IntSet.toList IntSet.fromList

instance Function a => Function (IntMap.IntMap a) where
  function = functionMap IntMap.toList IntMap.fromList

instance Function a => Function (Sequence.Seq a) where
  function = functionMap toList Sequence.fromList

#ifndef NO_PROXY
instance Function (Proxy a) where
  function = functionMap (const ()) (const Proxy)
#endif

#ifndef NO_NATURALS
instance Function Natural where
  function = functionIntegral
#endif

instance Function Int8 where
  function = functionBoundedEnum

instance Function Int16 where
  function = functionIntegral

instance Function Int32 where
  function = functionIntegral

instance Function Int64 where
  function = functionIntegral

instance Function Word8 where
  function = functionBoundedEnum

instance Function Word16 where
  function = functionIntegral

instance Function Word32 where
  function = functionIntegral

instance Function Word64 where
  function = functionIntegral

-- poly instances

instance Function A where
  function = functionMap unA A

instance Function B where
  function = functionMap unB B

instance Function C where
  function = functionMap unC C

instance Function OrdA where
  function = functionMap unOrdA OrdA

instance Function OrdB where
  function = functionMap unOrdB OrdB

instance Function OrdC where
  function = functionMap unOrdC OrdC

-- instance Arbitrary

instance (Function a, CoArbitrary a, Arbitrary b) => Arbitrary (a:->b) where
  arbitrary = function `fmap` arbitrary
  shrink    = shrinkFun shrink

--------------------------------------------------------------------------
-- generic function instances

#ifndef NO_GENERICS
-- | Generic 'Function' implementation.
genericFunction :: (Generic a, GFunction (Rep a)) => (a->b) -> (a:->b)
genericFunction = functionMapWith gFunction from to

class GFunction f where
  gFunction :: (f a -> b) -> (f a :-> b)

instance GFunction U1 where
  gFunction = functionMap (\U1 -> ()) (\() -> U1)

instance (GFunction f, GFunction g) => GFunction (f :*: g) where
  gFunction = functionMapWith (functionPairWith gFunction gFunction) g h
   where
     g (x :*: y) = (x, y)
     h (x, y) = x :*: y

instance (GFunction f, GFunction g) => GFunction (f :+: g) where
  gFunction = functionMapWith (functionEitherWith gFunction gFunction) g h
   where
     g (L1 x) = Left x
     g (R1 x) = Right x
     h (Left x) = L1 x
     h (Right x) = R1 x

instance GFunction f => GFunction (M1 i c f) where
  gFunction = functionMapWith gFunction (\(M1 x) -> x) M1

instance Function a => GFunction (K1 i a) where
  gFunction = functionMap (\(K1 x) -> x) K1
#endif

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
  [ p  .+. q' | q' <- shrinkFun shr q ] ++
  [ p' .+. q  | p' <- shrinkFun shr p ]
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

-- | Generation of random shrinkable, showable functions.
--
-- To generate random values of type @'Fun' a b@,
-- you must have an instance @'Function' a@.
--
-- See also 'apply' (and 'Fn' with GHC >= 7.8)
data Fun a b = Fun (a :-> b, b, Shrunk) (a -> b)
data Shrunk = Shrunk | NotShrunk deriving Eq

type Fun2 a b   = Fun (a, b)
type Fun3 a b c = Fun (a, b, c)

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 708
-- | A modifier for testing functions.
--
-- > prop :: Fun String Integer -> Bool
-- > prop (Fn f) = f "banana" == f "monkey"
-- >            || f "banana" == f "elephant"
#if __GLASGOW_HASKELL__ >= 800
pattern Fn :: (a -> b) -> Fun a b
#endif
pattern Fn f <- Fun _ f

-- | A modifier for testing binary functions.
--
-- @
--     prop_zipWith :: Fun (Int, Bool) Char -> [Int] -> [Bool] -> Bool
--     prop_zipWith (Fn2 f) xs ys = zipWith f xs ys == [ f x y | (x, y) <- zip xs ys]
-- @
--
-- >>> quickCheck prop_zipWith
-- +++ OK, passed 100 tests.
--
#if __GLASGOW_HASKELL__ >= 800
pattern Fn2 :: (a -> b -> c) -> Fun2 a b c
#endif
pattern Fn2 f <- Fun _ (curry -> f)

-- | A modifier for testing functions of three arguments.
#if __GLASGOW_HASKELL__ >= 800
pattern Fn3 :: (a -> b -> c -> d) -> Fun3 a b c d
#endif
pattern Fn3 f <- Fun _ (curry3 -> f)

curry3 f a b c = f (a, b, c)
#endif

mkFun :: (a :-> b) -> b -> Fun a b
mkFun p d = Fun (p, d, NotShrunk) (abstract p d)

-- | Alias to 'appFun'.
apply :: Fun a b -> (a -> b)
apply = appFun

-- | Extracts the function value.
--
-- 'Fn' is the pattern equivalent of this function.
--
-- > prop :: Fun String Integer -> Bool
-- > prop f = appFun f "banana" == appFun f "monkey"
-- >       || appFun f "banana" == appFun f "elephant"
appFun :: Fun a b -> (a -> b)
appFun (Fun _ f) = f

-- | Extracts the binary function value.
--
-- 'Fn3' is the pattern equivalent of the function.
appFun2 :: Fun (a, b) c -> (a -> b -> c)
appFun2 (Fun _ f) a b = f (a, b)

-- | Extracts the value of a function of three arguments. 'Fn3' is the
-- pattern equivalent of this function.
--
-- @
--     prop_zipWith :: Fun (Int, Bool) Char -> [Int] -> [Bool] -> Bool
--     prop_zipWith f xs ys = zipWith (apply f) xs ys == [ (apply f) x y | (x, y) <- zip xs ys]
-- @
--
appFun3 :: Fun (a, b, c) d -> (a -> b -> c -> d)
appFun3 (Fun _ f) a b c = f (a, b, c)

instance (Show a, Show b) => Show (Fun a b) where
  show (Fun (_, _, NotShrunk) _) = "<fun>"
  show (Fun (p, d, Shrunk) _)    = showFunction p (Just d)

instance (Function a, CoArbitrary a, Arbitrary b) => Arbitrary (Fun a b) where
  arbitrary =
    do p <- arbitrary
       d <- arbitrary
       return (mkFun p d)

  shrink (Fun (p, d, s) f) =
    [ mkFun p' d' | (p', d') <- shrink (p, d) ] ++
    [ Fun (p, d, Shrunk) f | s == NotShrunk ]

--------------------------------------------------------------------------
-- the end.
