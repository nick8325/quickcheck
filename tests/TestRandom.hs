-- Checking the quality of the random number generator,
-- in particular that splitting works OK.
-- Disabled by default as it's expensive and not testing QuickCheck as such.
{-# LANGUAGE BangPatterns #-}
import System.Random
import Test.QuickCheck.Random
import Control.Monad
import Data.List

-- A path is a sequence of splits - false represents the left path,
-- true represents the right path.
type Path = [Bool]

splits :: RandomGen a => Path -> a -> a
splits [] g = g
splits (False:xs) g = splits xs (fst (split g))
splits (True:xs) g = splits xs (snd (split g))

-- The properties we want to *falsify* are:
-- * two paths always generate the same result
-- * two paths always generate a different result
data Prop = Equal Path Path | Different Path Path
  deriving Show

paths :: Int -> [Path]
paths n = concat [sequence (replicate m [False, True]) | m <- [0..n]]

props :: Int -> [Prop]
props n =
  map (uncurry Equal) pairs ++ map (uncurry Different) pairs
  where
    ps = paths n
    pairs = [(p, q) | p <- ps, q <- ps, p < q]

supply :: RandomGen a => a -> [a]
supply = unfoldr (Just . split)

-- Generate the properties to check.
-- Parameters:
-- d = maximum depth of split,
-- k1 = range of number for first value
-- k2 = range of number for second value
check :: RandomGen a => Int -> Int -> Int -> a -> [Prop]
check d k1 k2 g =
  foldr filt (props d) gs
  where
    gs = take 10000 (supply g)
    filt g props = filter (eval g) props
    sample1 g = fst (randomR (0, k1) g)
    sample2 g = fst (randomR (0, k2) g)
    eval g (Equal xs ys) =
      sample1 (splits xs g) == sample2 (splits ys g)
    eval g (Different xs ys) =
      sample1 (splits xs g) /= sample2 (splits ys g)

-- First parameter: depth of splits to try
-- Second parameter: range of random numbers to generate
checkUpTo :: RandomGen a => Int -> Int -> a -> [(Int, Int, Int, Prop)]
checkUpTo d k g =
  [(d', k1', k2', prop)| d' <- [0..d], k1' <- [1..k], k2' <- [1..k], prop <- check d' k1' k2' g]

main = do
  g <- newQCGen
  let ![] = checkUpTo 6 20 g
  return ()
