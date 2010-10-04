{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
module Main where

--------------------------------------------------------------------------
-- imports

import Test.QuickCheck
import Test.QuickCheck.Text
import Test.QuickCheck.All
import Test.QuickCheck.Poly

import Data.List
  ( sort
  , (\\)
  )

import Control.Monad
  ( liftM
  , liftM2
  )

--------------------------------------------------------------------------
-- skew heaps

data Heap a
  = Node a (Heap a) (Heap a)
  | Nil
 deriving ( Eq, Ord, Show )
  
empty :: Heap a
empty = Nil

isEmpty :: Heap a -> Bool
isEmpty Nil = True
isEmpty _   = False

unit :: a -> Heap a
unit x = Node x empty empty

size :: Heap a -> Int
size Nil            = 0
size (Node _ h1 h2) = 1 + size h1 + size h2

insert :: Ord a => a -> Heap a -> Heap a
insert x h = unit x `merge` h

removeMin :: Ord a => Heap a -> Maybe (a, Heap a)
removeMin Nil            = Nothing
removeMin (Node x h1 h2) = Just (x, h1 `merge` h2)

merge :: Ord a => Heap a -> Heap a -> Heap a
h1  `merge` Nil = h1
Nil `merge` h2  = h2
h1@(Node x h11 h12) `merge` h2@(Node y h21 h22)
  | x <= y    = Node x (h12 `merge` h2) h11
  | otherwise = Node y (h22 `merge` h1) h21
        
fromList :: Ord a => [a] -> Heap a
fromList xs = merging [ unit x | x <- xs ]
 where
  merging []  = empty
  merging [h] = h
  merging hs  = merging (sweep hs) 

  sweep []         = []
  sweep [h]        = [h]
  sweep (h1:h2:hs) = (h1 `merge` h2) : sweep hs

toList :: Heap a -> [a]
toList h = toList' [h]
 where
  toList' []                  = []
  toList' (Nil          : hs) = toList' hs
  toList' (Node x h1 h2 : hs) = x : toList' (h1:h2:hs)

toSortedList :: Ord a => Heap a -> [a]
toSortedList Nil            = []
toSortedList (Node x h1 h2) = x : toList (h1 `merge` h2)

--------------------------------------------------------------------------
-- heap programs

data HeapP a
  = Empty
  | Unit a
  | Insert a (HeapP a)
  | SafeRemoveMin (HeapP a)
  | Merge (HeapP a) (HeapP a)
  | FromList [a]
 deriving (Show)

heap :: Ord a => HeapP a -> Heap a
heap Empty             = empty
heap (Unit x)          = unit x
heap (Insert x p)      = insert x (heap p)
heap (SafeRemoveMin p) = case removeMin (heap p) of
                           Nothing    -> empty -- arbitrary choice
                           Just (_,h) -> h
heap (Merge p q)       = heap p `merge` heap q
heap (FromList xs)     = fromList xs

instance Arbitrary a => Arbitrary (HeapP a) where
  arbitrary = sized arbHeapP
   where
    arbHeapP s =
      frequency
      [ (1, do return Empty)
      , (1, do x <- arbitrary
               return (Unit x))
      , (s, do x <- arbitrary
               p <- arbHeapP s1
               return (Insert x p))
      , (s, do p <- arbHeapP s1
               return (SafeRemoveMin p))
      , (s, do p <- arbHeapP s2
               q <- arbHeapP s2
               return (Merge p q))
      , (1, do xs <- arbitrary
               return (FromList xs))
      ]
     where
      s1 = s-1
      s2 = s`div`2


  shrink (Unit x)          = [ Unit x' | x' <- shrink x ]
  shrink (FromList xs)     = [ Unit x | x <- xs ]
                          ++ [ FromList xs' | xs' <- shrink xs ]
  shrink (Insert x p)      = [ p ]
                          ++ [ Insert x p' | p' <- shrink p ]
                          ++ [ Insert x' p | x' <- shrink x ]
  shrink (SafeRemoveMin p) = [ p ]
                          ++ [ SafeRemoveMin p' | p' <- shrink p ]
  shrink (Merge p q)       = [ p, q ]
                          ++ [ Merge p' q | p' <- shrink p ]
                          ++ [ Merge p q' | q' <- shrink q ]
  shrink _                 = []

data HeapPP a = HeapPP (HeapP a) (Heap a)
 deriving (Show)

instance (Ord a, Arbitrary a) => Arbitrary (HeapPP a) where
  arbitrary =
    do p <- arbitrary
       return (HeapPP p (heap p))

  shrink (HeapPP p _) =
    [ HeapPP p' (heap p') | p' <- shrink p ]

--------------------------------------------------------------------------
-- properties

(==?) :: Heap OrdA -> [OrdA] -> Bool
h ==? xs = sort (toList h) == sort xs

prop_Empty =
  empty ==? []

prop_IsEmpty (HeapPP _ h) =
  isEmpty h == null (toList h)

prop_Unit x =
  unit x ==? [x]

prop_Size (HeapPP _ h) =
  size h == length (toList h)

prop_Insert x (HeapPP _ h) =
  insert x h ==? (x : toList h)

prop_RemoveMin (HeapPP _ h) =
  cover (size h > 1) 80 "non-trivial" $
  case removeMin h of
    Nothing     -> h ==? []
    Just (x,h') -> x == minimum (toList h) && h' ==? (toList h \\ [x])

prop_Merge (HeapPP _ h1) (HeapPP _ h2) =
  (h1 `merge` h2) ==? (toList h1 ++ toList h2)

prop_FromList xs =
  fromList xs ==? xs

prop_ToSortedList (HeapPP _ h) =
  h ==? xs && xs == sort xs
 where
  xs = toSortedList h
  
--------------------------------------------------------------------------
-- main

main = $(quickCheckAll)

--------------------------------------------------------------------------
-- the end.

-- toSortedList (Node x h1 h2) = x : toSortedList (h1 `merge` h2)


