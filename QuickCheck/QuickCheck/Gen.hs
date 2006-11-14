module Chalmers.QuickCheck.Gen where

--------------------------------------------------------------------------
-- imports

import System.Random
  ( RandomGen(..)
  , Random(..)
  , StdGen
  , newStdGen
  )

import Control.Monad
  ( liftM
  )

import Control.Monad.Reader()
  -- needed for "instance Monad (a ->)"
  
  -- 2005-09-16:
  -- GHC gives a warning for this. I reported this as a bug. /Koen

--------------------------------------------------------------------------
-- type Gen

newtype Gen a = MkGen{ unGen :: StdGen -> Int -> a }

instance Functor Gen where
  fmap f (MkGen h) =
    MkGen (\r n -> f (h r n))

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
-- primitive generator combinators

variant :: Integral n => n -> Gen a -> Gen a
variant k (MkGen m) = MkGen (\r n -> m (var k r) n)
 where
  var k = (if k == k' then id  else var k')
        . (if even k  then fst else snd)
        . split
   where
    k' = k `div` 2

sized :: (Int -> Gen a) -> Gen a
sized f = MkGen (\r n -> let MkGen m = f n in m r n)

resize :: Int -> Gen a -> Gen a
resize n (MkGen m) = MkGen (\r _ -> m r n)

choose :: Random a => (a,a) -> Gen a
choose rng = MkGen (\r _ -> let (x,_) = randomR rng r in x)

promote :: Monad m => m (Gen a) -> Gen (m a)
promote m = MkGen (\r n -> liftM (\(MkGen m') -> m' r n) m)

sample :: Show a => Gen a -> IO ()
sample (MkGen m) =
  do rnd <- newStdGen
     let rnds rnd = rnd1 : rnds rnd2 where (rnd1,rnd2) = split rnd
     sequence_ [ print (m r n) | (r,n) <- rnds rnd `zip` [1..10] ]

--------------------------------------------------------------------------
-- common generator combinators

suchThat :: Gen a -> (a -> Bool) -> Gen a
gen `suchThat` p =
  do mx <- gen `suchThatMaybe` p
     case mx of
       Just x  -> return x
       Nothing -> sized (\n -> resize (n+1) (gen `suchThat` p))

suchThatMaybe :: Gen a -> (a -> Bool) -> Gen (Maybe a)
gen `suchThatMaybe` p = sized (try 0 . max 1) 
 where
  try _ 0 = return Nothing
  try k n = do x <- resize (2*k+n) gen
               if p x then return (Just x) else try (k+1) (n-1)

oneof :: [Gen a] -> Gen a
oneof [] = error "QuickCheck.oneof used with empty list"
oneof gs = choose (0,length gs - 1) >>= (gs !!)

frequency :: [(Int, Gen a)] -> Gen a
frequency [] = error "QuickCheck.frequency used with empty list"
frequency xs = choose (1, tot) >>= (`pick` xs)
 where
  tot = sum (map fst xs)

  pick n ((k,x):xs)
    | n <= k    = x
    | otherwise = pick (n-k) xs
  pick _ _  = error "QuickCheck.pick used with empty list"

elements :: [a] -> Gen a
elements [] = error "QuickCheck.elements used with empty list"
elements xs = (xs !!) `fmap` choose (0, length xs - 1)

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

listOf :: Gen a -> Gen [a]
listOf gen = sized $ \n ->
  do k <- choose (0,n)
     vectorOf k gen

listOf1 :: Gen a -> Gen [a]
listOf1 gen = sized $ \n ->
  do k <- choose (1,1 `max` n)
     vectorOf k gen

vectorOf :: Int -> Gen a -> Gen [a]
vectorOf k gen = sequence [ gen | _ <- [1..k] ]

--------------------------------------------------------------------------
-- the end.
