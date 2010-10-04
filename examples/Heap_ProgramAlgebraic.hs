{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
module Main where

--------------------------------------------------------------------------
-- imports

import Test.QuickCheck
import Test.QuickCheck.Text
import Test.QuickCheck.All
import Test.QuickCheck.Poly
import Test.QuickCheck.Property

import Data.List
  ( sort
  , nub
  , (\\)
  )

import Data.Maybe
  ( fromJust
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
fromList xs = merging [ unit x | x <- xs ] []
 where
  merging []       [] = empty
  merging [p]      [] = p
  merging (p:q:ps) qs = merging ps ((p`merge`q):qs)
  merging ps       qs = merging (ps ++ reverse qs) []

toList :: Heap a -> [a]
toList h = toList' [h]
 where
  toList' []                  = []
  toList' (Nil          : hs) = toList' hs
  toList' (Node x h1 h2 : hs) = x : toList' (h1:h2:hs)

toSortedList :: Ord a => Heap a -> [a]
toSortedList Nil            = []
toSortedList (Node x h1 h2) = x : toSortedList (h1 `merge` h2)

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

instance (Ord a, Arbitrary a) => Arbitrary (HeapP a) where
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


  shrink Empty         = []
  shrink (Unit x)      = [ Unit x' | x' <- shrink x ]
  shrink (FromList xs) = [ Unit x | x <- xs ]
                      ++ [ FromList xs' | xs' <- shrink xs ]
  shrink p             =
    [ FromList (toList (heap p)) ] ++
    case p of
      Insert x p      -> [ p ]
                      ++ [ Insert x p' | p' <- shrink p ]
                      ++ [ Insert x' p | x' <- shrink x ]
      SafeRemoveMin p -> [ p ]
                      ++ [ SafeRemoveMin p' | p' <- shrink p ]
      Merge p q       -> [ p, q ]
                      ++ [ Merge p' q | p' <- shrink p ]
                      ++ [ Merge p q' | q' <- shrink q ]

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

(=~) :: Heap Char -> Heap Char -> Property
{-
h1 =~ h2 = sort (toList h1) == sort (toList h2)
-}
h1 =~ h2 = property (nub (sort (toList h1)) == nub (sort (toList h2))) -- bug!


{-
The normal form is:

  insert x1 (insert x2 (... empty)...)

where x1 <= x2 <= ...
-}

-- heap creating operations

prop_Unit x =
  unit x =~ insert x empty

prop_RemoveMin_Empty =
  removeMin (empty :: Heap OrdA) == Nothing

prop_RemoveMin_Insert1 x =
  removeMin (insert x empty :: Heap OrdA) == Just (x, empty)

prop_RemoveMin_Insert2 x y (HeapPP _ h) =
  removeMin (insert x (insert y h)) ==~
    (insert (max x y) `maph` removeMin (insert (min x y) h))
 where
  f `maph` Just (x,h) = Just (x, f h)
  f `maph` Nothing    = Nothing

  Nothing     ==~ Nothing     = property True
  Just (x,h1) ==~ Just (y,h2) = x==y .&&. h1 =~ h2

prop_InsertSwap x y (HeapPP _ h) =
  insert x (insert y h) =~ insert y (insert x h)

prop_MergeInsertLeft x (HeapPP _ h1) (HeapPP _ h2) =
  (insert x h1 `merge` h2) =~ insert x (h1 `merge` h2)

prop_MergeInsertRight x (HeapPP _ h1) (HeapPP _ h2) =
  (h1 `merge` insert x h2) =~ insert x (h1 `merge` h2)

-- heap observing operations

prop_Size_Empty =
  size empty == 0

prop_Size_Insert x (HeapPP _ (h :: Heap OrdA)) =
  size (insert x h) == 1 + size h

prop_ToList_Empty =
  toList empty == ([] :: [OrdA])

prop_ToList_Insert x (HeapPP _ (h :: Heap OrdA)) =
  sort (toList (insert x h)) == sort (x : toList h)

prop_ToSortedList (HeapPP _ (h :: Heap OrdA)) =
  toSortedList h == sort (toList h)

--------------------------------------------------------------------------
-- main

main = $(quickCheckAll)

--------------------------------------------------------------------------
-- the end.



