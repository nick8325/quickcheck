{-# OPTIONS -fglasgow-exts #-}
module Main where

--------------------------------------------------------------------------
-- imports

import Test.QuickCheck

import Data.List
  ( sort
  )

--------------------------------------------------------------------------
-- merge sort

msort :: Ord a => [a] -> [a]
msort xs = merging [ [x] | x <- xs ]

merging :: Ord a => [[a]] -> [a]
merging []   = []
merging [xs] = xs
merging xss  = merging (sweep xss)

sweep :: Ord a => [[a]] -> [[a]]
sweep []          = []
sweep [xs]        = [xs]
sweep (xs:ys:xss) = merge xs ys : sweep xss

merge :: Ord a => [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys)
  | x <= y          = x : merge xs (y:ys)
  | otherwise       = y : merge (x:xs) ys

--------------------------------------------------------------------------
-- example properties

ordered :: Ord a => [a] -> Bool
ordered []       = True
ordered [x]      = True
ordered (x:y:xs) = x <= y && ordered (y:xs)

prop_Merge xs (ys :: [Int]) =
  ordered xs && ordered ys ==>
    collect (length xs + length ys) $
    ordered (xs `merge` ys)

--  collect (sort [length xs, length ys]) $



















--------------------------------------------------------------------------
-- quantificiation

--prop_Merge (Ordered xs) (Ordered (ys :: [Int])) =
--  ordered (xs `merge` ys)













--  classify (length xs `min` length ys >= 5) "not trivial" $
--  cover (length xs `min` length ys >= 5) 70 "not trivial" $

{-  
  shrink (Ordered xs) =
    [ Ordered xs'
    | xs' <- shrink xs
    , ordered xs'
    ]
-}

--------------------------------------------------------------------------
-- merging

prop_Merging (xss :: [OrderedList Int]) =
  ordered (merging [ xs | Ordered xs <- xss ])







--  mapSize (`div` 2) $ \(xss :: [OrderedList Int]) ->
  
--------------------------------------------------------------------------
-- the end.


