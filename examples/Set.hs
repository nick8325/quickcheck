{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
module Main where

--------------------------------------------------------------------------
-- imports

import Test.QuickCheck

import Text.Show.Functions
import Data.List
  ( sort
  , group
  , nub
  , (\\)
  )

import Control.Monad
  ( liftM
  , liftM2
  )

import Data.Maybe

--import Text.Show.Functions

--------------------------------------------------------------------------
-- binary search trees

data Set a
  = Node a (Set a) (Set a)
  | Empty
 deriving ( Eq, Ord, Show )

empty :: Set a
empty = Empty

isEmpty :: Set a -> Bool
isEmpty Empty = True
isEmpty _     = False

unit :: a -> Set a
unit x = Node x empty empty

size :: Set a -> Int
size Empty          = 0
size (Node _ s1 s2) = 1 + size s1 + size s2

insert :: Ord a => a -> Set a -> Set a
insert x s = s `union` unit x

merge :: Set a -> Set a -> Set a
s `merge` Empty                      = s
s `merge` Node x Empty s2            = Node x s s2
s `merge` Node x (Node y s11 s12) s2 = Node y s (Node x (s11 `merge` s12) s2)

delete :: Ord a => a -> Set a -> Set a
delete x Empty = Empty
delete x (Node x' s1 s2) =
  case x `compare` x' of
    LT -> Node x' (delete x s1) s2
    EQ -> s1 `merge` s2
    GT -> Node x' s1 (delete x s2)

union :: Ord a => Set a -> Set a -> Set a
{-
s1    `union` Empty = s1
Empty `union` s2    = s2
s1@(Node x s11 s12) `union` s2@(Node y s21 s22) =
  case x `compare` y of
    LT -> Node x s11 (s12 `union` Node y Empty s22) `union` s21
    EQ -> Node x (s11 `union` s21) (s12 `union` s22)
    --GT -> s11 `union` Node y s21 (Node x Empty s12 `union` s22)
    GT -> Node x (s11 `union` Node y s21 Empty) s12 `union` s22 
-}
s1             `union` Empty = s1
Empty          `union` s2    = s2
Node x s11 s12 `union` s2    = Node x (s11 `union` s21) (s12 `union` s22)
 where
  (s21,s22) = split x s2

split :: Ord a => a -> Set a -> (Set a, Set a)
split x Empty = (Empty, Empty)
split x (Node y s1 s2) =
  case x `compare` y of
    LT -> (s11, Node y s12 s2)
    EQ -> (s1, s2)
    GT -> (Node y s1 s21, s22)
 where
  (s11,s12) = split x s1
  (s21,s22) = split x s2
  
mapp :: (a -> b) -> Set a -> Set b
mapp f Empty          = Empty
mapp f (Node x s1 s2) = Node (f x) (mapp f s1) (mapp f s2)

fromList :: Ord a => [a] -> Set a
--fromList xs = build [ (empty,x) | x <- sort xs ]
fromList xs = build [ (empty,head x) | x <- group (sort xs) ]
 where
  build []      = empty
  build [(s,x)] = attach x s
  build sxs     = build (sweep sxs)

  sweep []                    = []
  sweep [sx]                  = [sx]
  sweep ((s1,x1):(s2,x2):sxs) = (Node x1 s1 s2,x2) : sweep sxs

  attach x Empty          = unit x
  attach x (Node y s1 s2) = Node y s1 (attach x s2)

toList :: Set a -> [a]
toList s = toSortedList s

toSortedList :: Set a -> [a]
toSortedList s = toList' s []
 where
  toList' Empty          xs = xs
  toList' (Node x s1 s2) xs = toList' s1 (x : toList' s2 xs)

--------------------------------------------------------------------------
-- generators

instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
  arbitrary = sized (arbSet Nothing Nothing)
   where
    arbSet mx my n =
      frequency $
        [ (1, return Empty) ] ++
        [ (7, do mz <- arbitrary `suchThatMaybe` (isOK mx my)
                 case mz of
                   Nothing -> return Empty
                   Just z  -> liftM2 (Node z) (arbSet mx mz n2)
                                              (arbSet mz my n2)
                    where n2 = n `div` 2)
        | n > 0
        ]

    isOK mx my z =
      maybe True (<z) mx && maybe True (z<) my

  shrink Empty            = []
  shrink t@(Node x s1 s2) = [ s1, s2 ]
                         ++ [ t' | x' <- shrink x, let t' = Node x' s1 s2, invariant t' ]

-- instance (Ord a, ShrinkSub a) => ShrinkSub (Set a)

--------------------------------------------------------------------------
-- properties

(.<) :: Ord a => Set a -> a -> Bool
Empty      .< x = True
Node y _ s .< x = y < x && s .< x

(<.) :: Ord a => a -> Set a -> Bool
x <. Empty      = True
x <. Node y _ s = x < y && x <. s

(==?) :: Ord a => Set a -> [a] -> Bool
s ==? xs = invariant s && sort (toList s) == nub (sort xs)

invariant :: Ord a => Set a -> Bool
invariant Empty          = True
invariant (Node x s1 s2) = s1 .< x && x <. s2 && invariant s1 && invariant s2

prop_Invariant (s :: Set Int) =
  invariant s

prop_Empty =
  empty ==? ([] :: [Int])

prop_Unit (x :: Int) =
  unit x ==? [x]

prop_Size (s :: Set Int) =
  cover (size s >= 15) 60 "large" $
    size s == length (toList s)

prop_Insert x (s :: Set Int) =
  insert x s ==? (x : toList s)

prop_Delete x (s :: Set Int) =
  delete x s ==? (toList s \\ [x])

prop_Union s1 (s2 :: Set Int) =
  (s1 `union` s2) ==? (toList s1 ++ toList s2)

prop_Mapp (f :: Int -> Int) (s :: Set Int) =
  expectFailure $
    whenFail (putStrLn ("Fun: " ++ show [ (x,f x) | x <- toList s])) $
      mapp f s ==? map f (toList s)

prop_FromList (xs :: [Int]) =
  fromList xs ==? xs

prop_ToSortedList (s :: Set Int) =
  s ==? xs && xs == sort xs
 where
  xs = toSortedList s
  
--  whenFail (putStrLn ("Result: " ++ show (fromList xs))) $

prop_FromList' (xs :: [Int]) =
  shrinking shrink xs $ \xs' ->
    fromList xs ==? xs

--------------------------------------------------------------------------
-- main

return []
main = $quickCheckAll

--------------------------------------------------------------------------
-- the end.
