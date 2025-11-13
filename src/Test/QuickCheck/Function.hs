{-# LANGUAGE TypeOperators, GADTs, CPP, Rank2Types #-}
#ifndef NO_SAFE_HASKELL
{-# LANGUAGE Trustworthy #-}
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
-- __Note__: most of the contents of this module are re-exported by
-- "Test.QuickCheck". You probably do not need to import it directly.
--
-- Example of use:
--
-- >>> :{
-- >>> let prop :: Fun String Integer -> Bool
-- >>>     prop (Fun _ f) = f "monkey" == f "banana" || f "banana" == f "elephant"
-- >>> :}
-- >>> quickCheck prop
-- *** Failed! Falsified (after 3 tests and 134 shrinks):
-- {"elephant"->1, "monkey"->1, _->0}
--
-- To generate random values of type @'Fun' a b@,
-- you must have an instance @'Function' a@.
-- If your type has a 'Show' instance, you can use 'functionShow' to write the instance; otherwise,
-- use 'functionMap' to give a bijection between your type and a type that is already an instance of 'Function'.
-- See the @'Function' [a]@ instance for an example of the latter.
module Test.QuickCheck.Function
  ( Fun(..)
  , mkFun
  , applyFun
  , apply
  , applyFun2
  , applyFun3
  , (:->)
  , Function(..)
  , functionMap
  , functionShow
  , functionIntegral
  , functionRealFrac
  , functionBoundedEnum
  , functionElements
  , functionVoid
  , functionMapWith
  , functionEitherWith
  , functionPairWith
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

import Control.Applicative
import Data.Char
import Data.Word
import Data.List( intersperse )
import Data.Ratio
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Sequence
import qualified Data.Tree as Tree
import Data.Int
import Data.Complex
import Data.Foldable(toList)
import Data.Functor.Identity
import qualified Data.Monoid as Monoid
import qualified Data.Semigroup as Semigroup
import qualified Data.List.NonEmpty as NonEmpty
import Numeric.Natural
import qualified Data.Bits as Bits
import Data.Tuple
import Data.Ord
import Data.Functor.Contravariant
import Text.Printf
import System.IO
import System.Exit
import Data.Version
import Data.Array.Byte
import qualified GHC.Exts as Exts

#if defined(MIN_VERSION_base)
import System.IO
  ( Newline(..)
  , NewlineMode(..)
  )
#endif

#ifndef NO_FIXED
import Data.Fixed
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
#if defined(__MHS__)
{- This is a temporary fix for a deficiency in the MicroHs type checker. -}
abstract (Pair p)    d xy    =
  case xy of
    (x,y) -> abstract (fmap (\q -> abstract q d y) p) d x
#else
abstract (Pair p)    d (x,y) = abstract (fmap (\q -> abstract q d y) p) d x
#endif
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

-- | The class @Function a@ is used for random generation of showable
-- functions of type @a -> b@.
--
-- There is a default implementation for 'function', which you can use
-- if your type has structural equality. Otherwise, you can normally
-- use 'functionMap' or 'functionShow'.
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
functionBoundedEnum = functionElements [minBound..maxBound]

-- | Provides a 'Function' instance for small finite types.
functionElements :: Eq a => [a] ->  (a->b) -> (a:->b)
functionElements xs f = Table [(x,f x) | x <- xs]

-- | Provides a 'Function' instance for types with 'RealFrac'.
functionRealFrac :: RealFrac a => (a->b) -> (a:->b)
functionRealFrac = functionMap toRational fromRational

-- | Provides a 'Function' instance for types with 'Integral'.
functionIntegral :: Integral a => (a->b) -> (a:->b)
functionIntegral = functionMap fromIntegral fromInteger

-- | Provides a 'Function' instance for types with 'Show' and 'Read'.
functionShow :: (Show a, Read a) => (a->c) -> (a:->c)
functionShow f = functionMap show read f

-- | Provides a 'Function' instance for types isomorphic to 'Data.Void.Void'.
--
-- An actual @'Function' 'Data.Void.Void'@ instance is defined in
-- @quickcheck-instances@.
functionVoid :: (forall b. void -> b) -> void :-> c
functionVoid _ = Nil

-- | The basic building block for 'Function' instances.
-- Provides a 'Function' instance by mapping to and from a type that
-- already has a 'Function' instance.
functionMap :: Function b => (a->b) -> (b->a) -> (a->c) -> (a:->c)
functionMap = functionMapWith function

-- | @since 2.13.3
functionMapWith :: ((b->c) -> (b:->c)) -> (a->b) -> (b->a) -> (a->c) -> (a:->c)
functionMapWith function g h f = Map g h (function (\b -> f (h b)))

instance Function () where
  function f = Unit (f ())

instance Function a => Function (Const a b) where
  function = functionMap getConst Const

instance Function a => Function (Identity a) where
  function = functionMap runIdentity Identity

instance (Function a, Function b) => Function (a,b) where
  function = functionPairWith function function

-- | @since 2.13.3
functionPairWith :: ((a->b->c) -> (a:->(b->c))) -> ((b->c) -> (b:->c)) -> ((a,b)->c) -> ((a,b):->c)
functionPairWith func1 func2 f = Pair (func2 `fmap` func1 (curry f))

instance (Function a, Function b) => Function (Either a b) where
  function = functionEitherWith function function

-- | @since 2.13.3
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

instance Function a => Function (NonEmpty.NonEmpty a) where
  function = functionMap (\(a NonEmpty.:| as) -> (a, as)) (\(a, as) -> a NonEmpty.:| as)

instance Function a => Function (ZipList a) where
  function = functionMap getZipList ZipList

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

instance Function Word where
  function = functionIntegral

instance Function Char where
  function = functionMap ord chr

instance Function Float where
  function = functionRealFrac

instance Function Double where
  function = functionRealFrac

instance Function Natural where
  function = functionIntegral

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

instance Function a => Function (Tree.Tree a) where
  function = functionMap (\(Tree.Node x xs) -> (x,xs)) (uncurry Tree.Node)

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

#if defined(MIN_VERSION_base)
instance Function Newline where
  function = functionMap g h
    where
      g LF = False
      g CRLF = True

      h False = LF
      h True = CRLF

instance Function NewlineMode where
  function = functionMap g h
    where
      g (NewlineMode inNL outNL) = (inNL,outNL)
      h (inNL,outNL) = NewlineMode inNL outNL
#endif

-- instances for Data.Monoid newtypes

instance Function a => Function (Monoid.Dual a) where
  function = functionMap Monoid.getDual Monoid.Dual

instance Function Monoid.All where
  function = functionMap Monoid.getAll Monoid.All

instance Function Monoid.Any where
  function = functionMap Monoid.getAny Monoid.Any

instance Function a => Function (Monoid.Sum a) where
  function = functionMap Monoid.getSum Monoid.Sum

instance Function a => Function (Monoid.Product a) where
  function = functionMap Monoid.getProduct Monoid.Product

instance Function a => Function (Monoid.First a) where
  function = functionMap Monoid.getFirst Monoid.First

instance Function a => Function (Monoid.Last a) where
  function = functionMap Monoid.getLast Monoid.Last

instance Function (f a) => Function (Monoid.Alt f a) where
  function = functionMap Monoid.getAlt Monoid.Alt

instance Function a => Function (Semigroup.Min a) where
  function = functionMap Semigroup.getMin Semigroup.Min

instance Function a => Function (Semigroup.Max a) where
  function = functionMap Semigroup.getMax Semigroup.Max

instance Function a => Function (Semigroup.Last a) where
  function = functionMap Semigroup.getLast Semigroup.Last

instance Function a => Function (Semigroup.First a) where
  function = functionMap Semigroup.getFirst Semigroup.First

instance Function a => Function (Semigroup.WrappedMonoid a) where
  function = functionMap Semigroup.unwrapMonoid Semigroup.WrapMonoid

instance (Function a, Function b) => Function (Semigroup.Arg a b) where
  function = functionMap (\(Semigroup.Arg a b) -> (a, b)) (uncurry Semigroup.Arg)

instance Function a => Function (Bits.And a) where
  function = functionMap Bits.getAnd Bits.And

instance Function a => Function (Bits.Ior a) where
  function = functionMap Bits.getIor Bits.Ior

instance Function a => Function (Bits.Xor a) where
  function = functionMap Bits.getXor Bits.Xor

instance Function a => Function (Bits.Iff a) where
  function = functionMap Bits.getIff Bits.Iff

instance Function FormatSign where
  function = functionMap (\x -> case x of SignPlus -> True; _ -> False) (\b -> if b then SignPlus else SignSpace)

instance Function FormatAdjustment where
  function = functionMap (\x -> case x of LeftAdjust -> True; _ -> False) (\b -> if b then LeftAdjust else ZeroPad)

instance Function FormatParse where
  function = functionMap to from
    where to fp = (fpModifiers fp, fpChar fp, fpRest fp)
          from (a, b, c) = FormatParse a b c

instance Function FieldFormat where
  function = functionMap to from
    where to ff = ( fmtWidth ff
                  , fmtPrecision ff
                  , fmtAdjust ff
                  , fmtSign ff
                  , fmtAlternate ff
                  , fmtModifiers ff
                  , fmtChar ff)
          from (a, b, c, d, e, f, g) = FieldFormat a b c d e f g

instance Function GeneralCategory where
  function = functionBoundedEnum

instance Function SeekMode where
  function = functionElements [AbsoluteSeek, RelativeSeek, SeekFromEnd]

instance Function IOMode where
  function = functionElements [ReadMode, WriteMode, AppendMode, ReadWriteMode]

instance Function BufferMode where
  function = functionMap to from
    where to NoBuffering = Left True
          to LineBuffering = Left False
          to (BlockBuffering m) = Right m

          from (Left True) = NoBuffering
          from (Left False) = LineBuffering
          from (Right m)    = BlockBuffering m

instance Function ExitCode where
  function = functionMap to from
    where to ExitSuccess = Nothing
          to (ExitFailure c) = Just c

          from Nothing = ExitSuccess
          from (Just c) = ExitFailure c

instance Function Version where
  function = functionMap to from
    where to (Version a b) = (a, b)
          from (a, b) = Version a b

instance Function ByteArray where
  function = functionMap Exts.toList Exts.fromList

#if MIN_VERSION_base(4,16,0)

#if !MIN_VERSION_base(4,18,0)

getSolo :: Solo a -> a
getSolo (Solo a) = a

mkSolo :: a -> Solo a
mkSolo = Solo

#elif !MIN_VERSION_base(4,19,0)

getSolo :: Solo a -> a
getSolo (MkSolo a) = a

mkSolo :: a -> Solo a
mkSolo = MkSolo

#else

mkSolo :: a -> Solo a
mkSolo = MkSolo

#endif

instance Function a => Function (Solo a) where
  function = functionMap getSolo mkSolo

#endif

instance Function a => Function (Down a) where
  function = functionMap getDown Down


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

  table :: Eq aa => [(aa,cc)] -> (aa :-> cc) -- MicroHs needs this
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
-- See also 'applyFun', and 'Fn' with GHC >= 7.8.
data Fun a b = Fun (a :-> b, b, Shrunk) (a -> b)
data Shrunk = Shrunk | NotShrunk deriving Eq

instance Functor (Fun a) where
  fmap f (Fun (p, d, s) g) = Fun (fmap f p, f d, s) (f . g)

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 708
-- | A modifier for testing functions.
--
-- > prop :: Fun String Integer -> Bool
-- > prop (Fn f) = f "banana" == f "monkey"
-- >            || f "banana" == f "elephant"
#if __GLASGOW_HASKELL__ >= 800
pattern Fn :: (a -> b) -> Fun a b
#endif
pattern Fn f <- (applyFun -> f)
#if __GLASGOW_HASKELL__ >= 802
{-# COMPLETE Fn #-}
#endif

-- | A modifier for testing binary functions.
--
-- > prop_zipWith :: Fun (Int, Bool) Char -> [Int] -> [Bool] -> Bool
-- > prop_zipWith (Fn2 f) xs ys = zipWith f xs ys == [ f x y | (x, y) <- zip xs ys]
#if __GLASGOW_HASKELL__ >= 800
pattern Fn2 :: (a -> b -> c) -> Fun (a, b) c
#endif
pattern Fn2 f <- (applyFun2 -> f)
#if __GLASGOW_HASKELL__ >= 802
{-# COMPLETE Fn2 #-}
#endif

-- | A modifier for testing ternary functions.
#if __GLASGOW_HASKELL__ >= 800
pattern Fn3 :: (a -> b -> c -> d) -> Fun (a, b, c) d
#endif
pattern Fn3 f <- (applyFun3 -> f)
#if __GLASGOW_HASKELL__ >= 802
{-# COMPLETE Fn3 #-}
#endif
#endif

-- | Create a `Fun` from a function representation and a default value (in case the function
-- is partial).
mkFun :: (a :-> b) -> b -> Fun a b
mkFun p d = Fun (p, d, NotShrunk) (abstract p d)

-- | Alias to 'applyFun'.
apply :: Fun a b -> (a -> b)
apply = applyFun

-- | Extracts the value of a function.
--
-- 'Fn' is the pattern equivalent of this function.
--
-- > prop :: Fun String Integer -> Bool
-- > prop f = applyFun f "banana" == applyFun f "monkey"
-- >       || applyFun f "banana" == applyFun f "elephant"
applyFun :: Fun a b -> (a -> b)
applyFun (Fun _ f) = f

-- | Extracts the value of a binary function.
--
-- 'Fn2' is the pattern equivalent of this function.
--
--  > prop_zipWith :: Fun (Int, Bool) Char -> [Int] -> [Bool] -> Bool
--  > prop_zipWith f xs ys = zipWith (applyFun2 f) xs ys == [ applyFun2 f x y | (x, y) <- zip xs ys]
--
applyFun2 :: Fun (a, b) c -> (a -> b -> c)
applyFun2 (Fun _ f) a b = f (a, b)

-- | Extracts the value of a ternary function. 'Fn3' is the
-- pattern equivalent of this function.
applyFun3 :: Fun (a, b, c) d -> (a -> b -> c -> d)
applyFun3 (Fun _ f) a b c = f (a, b, c)

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
