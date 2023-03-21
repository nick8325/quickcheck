module Main where

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Control.Concurrent

{-

A synthetic shrinking example to illustrate how parallel shrinking may
speed up execution.

  200 F
    |
  [300 T, 200 T, 200 T, 740 T, 200 F, 200 T, ...]
                                 |
                              [300 T, 200 F, ...]
                                        |
                                      [600 T, 200 T, 234 T, 543 T, 342 F, ...]
                                                                      |
                                                                    .....




-}

--              delay
--                |
--                |
--                | success
--                |   |
--                |   |
--                |   |    shrinks
--                v   v      v
data Tree = Node Int Bool [Tree]
  deriving Show

instance Arbitrary Tree where
    arbitrary = genTree
    shrink (Node _ _ xs) = xs

depth :: (Int, Int)
depth = (3,30)

delays :: (Int, Int)
delays = (150000, 200000)

width :: (Int, Int)
width = (0,30)

genTree :: Gen Tree
genTree = do
    depth <- chooseInt depth
    xs <- genShrinkList depth 0
    return $ head xs

genShrinkList :: Int -> Int -> Gen [Tree]
genShrinkList 0 _ = return $ [Node 0 False []]
genShrinkList d i = do
--    successes <- sequence $ replicate i $ genSuccessfulNode 
    successes <- sequence $ replicate i $ frequency [ (19, genSuccessfulNode)
                                                    , (1, do delay <- chooseInt delays
                                                             nextix <- chooseInt width
                                                             shrinks <- genShrinkList (d-1) nextix
                                                             return $ Node delay False shrinks)]--replicate i genSuccessfulNode
    delay <- chooseInt delays
    nextix <- chooseInt width
    shrinks <- genShrinkList (d-1) nextix
    return $ successes ++ [Node delay False shrinks]

genSuccessfulNode :: Gen Tree
genSuccessfulNode = do
    delay <- chooseInt delays
    return $ Node delay True []

prop_tree :: Tree -> Property
prop_tree (Node delay b _) = monadicIO $ do
    run $ threadDelay delay
    assert b

main = quickCheckPar prop_tree
