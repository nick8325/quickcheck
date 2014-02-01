{-# LANGUAGE CPP #-}
#ifndef NO_ST_MONAD
{-# LANGUAGE Rank2Types #-}
#endif
-- | Test case generation.
module Test.QuickCheck.Gen where

--------------------------------------------------------------------------
-- imports

import System.Random
  ( Random
  , StdGen
  , randomR
  , split
  , newStdGen
  )

import Control.Monad
  ( liftM
  , ap
  )

import Control.Applicative
  ( Applicative(..)
  )

import Test.QuickCheck.Random

--------------------------------------------------------------------------
-- ** Generator type

newtype Gen a = MkGen{ unGen :: QCGen -> Int -> a }

instance Functor Gen where
  fmap f (MkGen h) =
    MkGen (\r n -> f (h r n))

instance Applicative Gen where
  pure  = return
  (<*>) = ap

instance Monad Gen where
  return x =
    MkGen (\_ _ -> x)

  MkGen m >>= k =
    MkGen (\r n ->
      let (r1,r2)  = split r
          MkGen m' = k (m r1 n)
       in m' r2 n
    )

--------------------------------------------------------------------------
-- ** Primitive generator combinators

-- | Modifies a generator using an integer seed.
variant :: Integral n => n -> Gen a -> Gen a
variant k0 (MkGen m) = MkGen (\r n -> m (var k0 r) n)
 where
  var k = (if k == k' then id  else var k')
        . (if even k  then fst else snd)
        . split
   where
    k' = k `div` 2

-- | Used to construct generators that depend on the size parameter.
sized :: (Int -> Gen a) -> Gen a
sized f = MkGen (\r n -> let MkGen m = f n in m r n)

-- | Overrides the size parameter. Returns a generator which uses
-- the given size instead of the runtime-size parameter.
resize :: Int -> Gen a -> Gen a
resize n (MkGen m) = MkGen (\r _ -> m r n)

-- | Generates a random element in the given inclusive range.
choose :: Random a => (a,a) -> Gen a
choose rng = MkGen (\r _ -> let (x,_) = randomR rng r in x)

-- | Allows you to re-use the same seed several times.
-- Unsafe: careless use may break the monad laws!
delay :: Gen (Gen a -> a)
delay = MkGen (\r n g -> unGen g r n)

#ifndef NO_ST_MONAD
-- | A variant of 'delay' that returns a rank-2 type.
delay' :: Gen Delay
delay' = MkGen (\r n -> Delay (\g -> unGen g r n))

newtype Delay = Delay (forall a. Gen a -> a)
#endif

-- | Promotes a monadic generator to a generator of monadic values.
-- Unsafe: careless use may break the monad laws!
promote :: Monad m => m (Gen a) -> Gen (m a)
promote m = do
  f <- delay
  return (liftM f m)

-- | Generates an infinite list of example values.
samples :: Gen a -> IO [a]
samples (MkGen m) =
  do rnd0 <- newQCGen
     let rnds rnd = rnd1 : rnds rnd2 where (rnd1,rnd2) = split rnd
     return [(m r n) | (r,n) <- rnds rnd0 `zip` [0,2..] ]

-- | Generates some example values.
sample' :: Gen a -> IO [a]
sample' g = samples g >>= return . take 11

-- | Generates some example values and prints them to 'stdout'.
sample :: Show a => Gen a -> IO ()
sample g =
  do cases <- sample' g
     sequence_ (map print cases)

--------------------------------------------------------------------------
-- ** Common generator combinators

-- | Generates a value that satisfies a predicate.
suchThat :: Gen a -> (a -> Bool) -> Gen a
gen `suchThat` p =
  do mx <- gen `suchThatMaybe` p
     case mx of
       Just x  -> return x
       Nothing -> sized (\n -> resize (n+1) (gen `suchThat` p))

-- | Tries to generate a value that satisfies a predicate.
suchThatMaybe :: Gen a -> (a -> Bool) -> Gen (Maybe a)
gen `suchThatMaybe` p = sized (try 0 . max 1)
 where
  try _ 0 = return Nothing
  try k n = do x <- resize (2*k+n) gen
               if p x then return (Just x) else try (k+1) (n-1)

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

-- | Takes a list of elements of increasing size, and chooses
-- among an initial segment of the list. The size of this initial
-- segment increases with the size parameter.
-- The input list must be non-empty.
growingElements :: [a] -> Gen a
growingElements [] = error "QuickCheck.growingElements used with empty list"
growingElements xs = sized $ \n -> elements (take (1 `max` size n) xs)
  where
   k      = length xs
   mx     = 100
   log'   = round . log . fromIntegral
   size n = (log' n + 1) * k `div` log' mx

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
vectorOf k gen = sequence [ gen | _ <- [1..k] ]

--------------------------------------------------------------------------
-- the end.
