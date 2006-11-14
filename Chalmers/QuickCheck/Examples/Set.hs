{-# OPTIONS -fglasgow-exts #-}
module Set where

import Data.FiniteMap

import Maybe
  ( isJust
  )
  
-------------------------------------------------------------------------
-- maps and sets

type a :=> b = FiniteMap a b
type Set a   = a :=> ()

{-
instance (Show a, Show b) => Show (a :=> b) where
  showsPrec n x = showsPrec n (tabulate x)
-}

-------------------------------------------------------------------------
-- interface

empty :: (a :=> b)
empty = emptyFM

isEmpty :: (a :=> b) -> Bool
isEmpty tab = size tab == 0

pair :: a -> b -> (a :=> b)
pair x y = unitFM x y

look :: Ord a => a -> (a :=> b) -> Maybe b
look x tab = lookupFM tab x

union :: Ord a => (a :=> b) -> (a :=> b) -> (a :=> b)
tab1 `union` tab2 = plusFM tab1 tab2

unionWith :: Ord a => (b -> b -> b) -> (a :=> b) -> (a :=> b) -> (a :=> b)
unionWith op tab1 tab2 = plusFM_C op tab1 tab2

delete :: Ord a => a -> (a :=> b) -> (a :=> b)
x `delete` tab = delFromFM tab x

difference :: Ord a => (a :=> b) -> (a :=> b) -> (a :=> b)
tab1 `difference` tab2 = minusFM tab1 tab2

intersect :: Ord a => (a :=> b) -> (a :=> b) -> (a :=> b)
tab1 `intersect` tab2 = intersectFM tab1 tab2

extend :: Ord a => a -> b -> (a :=> b) -> (a :=> b)
extend x y tab = addToFM tab x y

member, notMember :: Ord a => a -> (a :=> b) -> Bool
x `member`    tab = isJust (look x tab)
x `notMember` tab = not (x `member` tab)

subset :: Ord a => (a :=> b) -> (a :=> b) -> Bool
xs `subset` ys = size (xs `difference` ys) == 0

tabulate :: (a :=> b) -> [(a,b)]
tabulate tab = fmToList tab

table :: Ord a => [(a,b)] -> (a :=> b)
table xys = listToFM xys

accumulate :: Ord a => (b -> b -> b) -> [(a,b)] -> (a :=> b)
accumulate op xys = addListToFM_C op empty xys

size :: (a :=> b) -> Int
size = sizeFM

pick :: Ord a => (a :=> b) -> Maybe ((a,b),a :=> b)
pick tab | size tab == 0 = Nothing
         | otherwise     = Just ((a,b), a `delete` tab)
 where
  ((a,b):_) = tabulate tab

mapElt :: Ord a => (b -> c) -> (a :=> b) -> (a :=> c)
mapElt f tab = mapFM (\_ -> f) tab
 
-------------------------------------------------------------------------
-- set specific

set :: Ord a => [a] -> Set a
set xs = listToFM [ (x,()) | x <- xs ]

insert :: Ord a => a -> Set a -> Set a
x `insert` set = extend x () set

unit :: a -> Set a
unit x = unitFM x ()

elements :: (a :=> b) -> [a]
elements set = keysFM set

-------------------------------------------------------------------------
-- the end.
