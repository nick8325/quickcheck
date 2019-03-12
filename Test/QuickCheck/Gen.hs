{-# LANGUAGE CPP #-}
#ifndef NO_ST_MONAD
{-# LANGUAGE Rank2Types #-}
#endif
-- | Test case generation.
--
-- __Note__: the contents of this module (except for the definition of
-- 'Gen') are re-exported by "Test.QuickCheck". You probably do not
-- need to import it directly.
module Test.QuickCheck.Gen where

--------------------------------------------------------------------------
-- imports

import System.Random
  ( Random
  , random
  , randomR
  , split
  )

import Control.Monad
  ( ap
  , replicateM
  , filterM
  )

import Control.Monad.Fix
  ( MonadFix(..) )

import Control.Applicative
  ( Applicative(..) )

import Test.QuickCheck.Random
import Data.List
import Data.Ord
import Data.Maybe

--------------------------------------------------------------------------
-- ** Generator type

-- | A generator for values of type @a@.
--
-- The third-party packages
-- <http://hackage.haskell.org/package/QuickCheck-GenT QuickCheck-GenT>
-- and
-- <http://hackage.haskell.org/package/quickcheck-transformer quickcheck-transformer>
-- provide monad transformer versions of @Gen@.
newtype Gen a = MkGen{
  unGen :: QCGen -> Int -> a -- ^ Run the generator on a particular seed.
                             -- If you just want to get a random value out, consider using 'generate'.
  }

instance Functor Gen where
  fmap f (MkGen h) =
    MkGen (\r n -> f (h r n))

instance Applicative Gen where
  pure  = return
  gf <*> gx = gf >>= \f -> fmap f gx

instance Monad Gen where
  return x =
    MkGen (\_ _ -> x)

  MkGen m >>= k =
    MkGen (\r n ->
      case split r of
        (r1, r2) ->
          let MkGen m' = k (m r1 n)
          in m' r2 n
    )

instance MonadFix Gen where
  mfix f =
    MkGen $ \r n ->
      let a = unGen (f a) r n
      in a

--------------------------------------------------------------------------
-- ** Primitive generator combinators

-- | Modifies a generator using an integer seed.
variant :: Integral n => n -> Gen a -> Gen a
variant k (MkGen g) = MkGen (\r n -> g (variantQCGen k r) n)

-- | Used to construct generators that depend on the size parameter.
--
-- For example, 'listOf', which uses the size parameter as an upper bound on
-- length of lists it generates, can be defined like this:
--
-- > listOf :: Gen a -> Gen [a]
-- > listOf gen = sized $ \n ->
-- >   do k <- choose (0,n)
-- >      vectorOf k gen
--
-- You can also do this using 'getSize'.
sized :: (Int -> Gen a) -> Gen a
sized f = MkGen (\r n -> let MkGen m = f n in m r n)

-- | Returns the size parameter. Used to construct generators that depend on
-- the size parameter.
--
-- For example, 'listOf', which uses the size parameter as an upper bound on
-- length of lists it generates, can be defined like this:
--
-- > listOf :: Gen a -> Gen [a]
-- > listOf gen = do
-- >   n <- getSize
-- >   k <- choose (0,n)
-- >   vectorOf k gen
--
-- You can also do this using 'sized'.
getSize :: Gen Int
getSize = sized pure

-- | Overrides the size parameter. Returns a generator which uses
-- the given size instead of the runtime-size parameter.
resize :: Int -> Gen a -> Gen a
resize n _ | n < 0 = error "Test.QuickCheck.resize: negative size"
resize n (MkGen g) = MkGen (\r _ -> g r n)

-- | Adjust the size parameter, by transforming it with the given
-- function.
scale :: (Int -> Int) -> Gen a -> Gen a
scale f g = sized (\n -> resize (f n) g)

-- | Generates a random element in the given inclusive range.
choose :: Random a => (a,a) -> Gen a
choose rng = MkGen (\r _ -> let (x,_) = randomR rng r in x)

-- | Curried version of `choose`.
cchoose :: Random a => a -> a -> Gen a
cchoose = curry choose

-- | Generates a random element over the natural range of `a`.
chooseAny :: Random a => Gen a
chooseAny = MkGen (\r _ -> let (x,_) = random r in x)

-- | Run a generator. The size passed to the generator is always 30;
-- if you want another size then you should explicitly use 'resize'.
generate :: Gen a -> IO a
generate (MkGen g) =
  do r <- newQCGen
     return (g r 30)

-- | Generates some example values.
sample' :: Gen a -> IO [a]
sample' g =
  generate (sequence [ resize n g | n <- [0,2..20] ])

-- | Generates some example values and prints them to 'stdout'.
sample :: Show a => Gen a -> IO ()
sample g =
  do cases <- sample' g
     mapM_ print cases

--------------------------------------------------------------------------
-- ** Common generator combinators

-- | Generates a value that satisfies a predicate.
suchThat :: Gen a -> (a -> Bool) -> Gen a
gen `suchThat` p =
  do mx <- gen `suchThatMaybe` p
     case mx of
       Just x  -> return x
       Nothing -> sized (\n -> resize (n+1) (gen `suchThat` p))

-- | Generates a value for which the given function returns a 'Just', and then
-- applies the function.
suchThatMap :: Gen a -> (a -> Maybe b) -> Gen b
gen `suchThatMap` f =
  fmap fromJust $ fmap f gen `suchThat` isJust

-- | Tries to generate a value that satisfies a predicate.
-- If it fails to do so after enough attempts, returns @Nothing@.
suchThatMaybe :: Gen a -> (a -> Bool) -> Gen (Maybe a)
gen `suchThatMaybe` p = sized (\n -> try n (2*n))
 where
  try m n
    | m > n = return Nothing
    | otherwise = do
        x <- resize m gen
        if p x then return (Just x) else try (m+1) n

-- | Randomly uses one of the given generators. The input list
-- must be non-empty.
oneof :: [Gen a] -> Gen a
oneof [] = error "QuickCheck.oneof used with empty list"
oneof gs = choose (0,length gs - 1) >>= (gs !!)

-- | Chooses one of the given generators, with a weighted random distribution.
-- The input list must be non-empty.
frequency :: [(Int, Gen a)] -> Gen a
frequency [] = error "QuickCheck.frequency used with empty list"
frequency xs0 = choose (1, tot) >>= (`pick` xs0)
 where
  tot = sum (map fst xs0)

  pick n ((k,x):xs)
    | n <= k    = x
    | otherwise = pick (n-k) xs
  pick _ _  = error "QuickCheck.pick used with empty list"

-- | Generates one of the given values. The input list must be non-empty.
elements :: [a] -> Gen a
elements [] = error "QuickCheck.elements used with empty list"
elements xs = (xs !!) `fmap` choose (0, length xs - 1)

-- | Generates a random subsequence of the given list.
sublistOf :: [a] -> Gen [a]
sublistOf xs = filterM (\_ -> choose (False, True)) xs

-- | Generates a random permutation of the given list.
shuffle :: [a] -> Gen [a]
shuffle xs = do
  ns <- vectorOf (length xs) (choose (minBound :: Int, maxBound))
  return (map snd (sortBy (comparing fst) (zip ns xs)))

-- | Takes a list of elements of increasing size, and chooses
-- among an initial segment of the list. The size of this initial
-- segment increases with the size parameter.
-- The input list must be non-empty.
growingElements :: [a] -> Gen a
growingElements [] = error "QuickCheck.growingElements used with empty list"
growingElements xs = sized $ \n -> elements (take (1 `max` size n) xs)
  where
   k        = length xs
   mx       = 100
   log'     = round . log . toDouble
   size n   = (log' n + 1) * k `div` log' mx
   toDouble = fromIntegral :: Int -> Double

{- WAS:
growingElements xs = sized $ \n -> elements (take (1 `max` (n * k `div` 100)) xs)
 where
  k = length xs
-}

-- | Generates a list of random length. The maximum length depends on the
-- size parameter.
listOf :: Gen a -> Gen [a]
listOf gen = sized $ \n ->
  do k <- choose (0,n)
     vectorOf k gen

-- | Generates a non-empty list of random length. The maximum length
-- depends on the size parameter.
listOf1 :: Gen a -> Gen [a]
listOf1 gen = sized $ \n ->
  do k <- choose (1,1 `max` n)
     vectorOf k gen

-- | Generates a list of the given length.
vectorOf :: Int -> Gen a -> Gen [a]
vectorOf = replicateM

-- | Generates an infinite list.
infiniteListOf :: Gen a -> Gen [a]
infiniteListOf gen = sequence (repeat gen)

--------------------------------------------------------------------------
-- the end.
