module Main where

--------------------------------------------------------------------------
-- imports

--import Debug.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Text

import Data.Generics

import Data.List
  ( sort
  , group
  , nub
  )

import Control.Monad
  ( liftM
  , liftM2
  , MonadPlus(..)
  )

import Data.Maybe
  ( isJust
  )

--------------------------------------------------------------------------
-- paths

data P
  = Choose Int P
  | P `Then` P
  | Done

class Path a where
  path :: a -> P

  path{| Unit |}    Unit      = Done
  path{| a :*: b |} (x :*: y) = path x `Then` path y
  path{| a :+: b |} (Inl x)   = 0 `Choose` path x
  path{| a :+: b |} (Inr y)   = 1 `Choose` path y

instance Path Int where
  path n = (if n >= 0 then 0 else 1) `Choose` pathN (abs n)
   where
    pathN 0 = Done
    pathN k = (k `mod` 10) `Choose` pathN (k `div` 10)

instance Path a => Path [a]
instance (Path a,Path b) => Path (a,b)

--------------------------------------------------------------------------
-- tries

data Trie a
  = Node (Maybe a) [Trie a]
 deriving ( Eq, Show )
  
empty :: Trie a
empty = Node Nothing []

isEmpty :: Trie a -> Bool
isEmpty (Node Nothing ts) = null ts -- all isEmpty ts
isEmpty (Node (Just _) _) = False

unit :: Path a => a -> Trie a
unit x = trie (path x) x
 where
  trie []     x = Node (Just x) []
--  trie (i:is) x = Node Nothing (replicate (i-1) empty ++ [trie is x])
  trie (i:is) x = Node Nothing (replicate i empty ++ [trie is x])

member, notMember :: Path a => a -> Trie a -> Bool
x `notMember` t = not (x `member` t)
x `member` t = search (path x) t
 where
  search []     (Node mx _) = isJust mx -- mx == Just x
  search (i:is) (Node _ ts) = searchi i is ts
  
  searchi _ _  []     = False
  searchi 0 is (t:_)  = search is t
  searchi i is (_:ts) = searchi (i-1) is ts

size :: Trie a -> Int
size (Node mx ts) = maybe 0 (const 1) mx + sum (map size ts)

insert :: Path a => a -> Trie a -> Trie a
insert x s = s `union` unit x

union :: Trie a -> Trie a -> Trie a
Node mx1 ts1 `union` Node mx2 ts2 =
  Node (mx1 `mplus` mx2) (ts1 `union'` ts2)
 where
  ts1      `union'` []       = ts1
  []       `union'` ts2      = ts2
  (t1:ts1) `union'` (t2:ts2) = (t1 `union` t2) : (ts1 `union'` ts2)

fromList :: Path a => [a] -> Trie a
fromList xs = build [ unit x | x <- xs ]
 where
  build []  = empty
  build [t] = t
  build ts  = build (sweep ts)

  sweep []         = []
  sweep [t]        = [t]
  sweep (t1:t2:ts) = (t1 `union` t2) : sweep ts

toList :: Trie a -> [a]
toList t = toList' [t]
 where
  toList' []                 = []
  toList' (Node mx ts : ts') = [ x | Just x <- [mx] ] ++ toList' (ts ++ ts')

--------------------------------------------------------------------------
-- generators

instance (Path a, Arbitrary a) => Arbitrary (Trie a) where
  arbitrary = sized arbTrie
   where
    arbTrie n =
      frequency $
        [ (1, return empty)
        , (1, liftM unit arbitrary)
        , (1, liftM fromList arbitrary)
        ] ++
        concat
        [ [ (4, liftM2 insert arbitrary arbTrie1)
          , (5, liftM2 union arbTrie2 arbTrie2)
          ]
        | n > 0
        ]
     where
      arbTrie1 = arbTrie (n-1)
      arbTrie2 = arbTrie (n `div` 2) 

  shrink (Node mx ts) =
    [ Node Nothing ts | Just _ <- [mx] ] ++
    [ Node mx     ts' | ts' <- shrinkToEmpty ts ]
   where
    shrinkToEmpty []     = []
    shrinkToEmpty (t:ts) = [ empty : ts | not (isEmpty t) ]
                        ++ [ t:ts' | ts' <- shrinkToEmpty ts ]

--------------------------------------------------------------------------
-- properties

type T = (Int,Int)

(==?) :: (Ord a, Path a) => Trie a -> [a] -> Bool
s ==? xs = invariant s && sort (toList s) == nub (sort xs)

invariant :: (Ord a, Path a) => Trie a -> Bool
invariant t = inv [] t
 where
  inv p (Node mx ts) =
       maybe True ((p ==) . path) mx
    && and [ inv (p ++ [i]) t | (i,t) <- [0..] `zip` ts ]

prop_Empty =
  empty ==? ([] :: [T])

prop_IsEmpty (s :: Trie T) =
  isEmpty s == null (toList s)

prop_Unit (x :: T) =
  unit x ==? [x]

prop_Size (s :: Trie T) =
  cover (size s >= 15) 50 "large" $
    size s == length (toList s)

prop_Member x (s :: Trie T) =
  (x `member` s) == (x `elem` toList s)

prop_Insert x (s :: Trie T) =
  insert x s ==? (x : toList s)

prop_Union s1 (s2 :: Trie T) =
  (s1 `union` s2) ==? (toList s1 ++ toList s2)

prop_Union' =
  forThis (unit (1::Int,-11)) $ \s1 ->
    forThis (unit (11,-1::Int)) $ \s2 ->
      (s1 `union` s2) ==? (toList s1 ++ toList s2)

prop_Paths (x,y) (p::Int,q::Int) =
  x /= p && y /= q ==>
    path (x,y) /= path (p,q)

prop_FromList (xs :: [T]) =
  whenFail (putStrLn ("Result: " ++ show (fromList xs))) $
  fromList xs ==? xs

--------------------------------------------------------------------------
-- main

main =
  do --quickCheck prop_Empty
     --quickCheck prop_IsEmpty
     --quickCheck prop_Unit
     --quickCheck prop_Size
     --quickCheck prop_Member
     --quickCheck prop_Insert
     --quickCheck prop_Union
     quickCheck prop_Union'
     quickCheck prop_Paths
     print (path (1::Int,-11::Int), path(11::Int,-1::Int))
     --quickCheck prop_FromList

--------------------------------------------------------------------------
-- the end.
