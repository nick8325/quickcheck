{-# OPTIONS_HADDOCK hide #-}
module Test.QuickCheck.Features where

import Test.QuickCheck.Property hiding (Result, reason)
import qualified Test.QuickCheck.Property as P
import Test.QuickCheck.Test
import Test.QuickCheck.Gen
import Test.QuickCheck.Text
import qualified Data.Set as Set
import Data.Set(Set)
import Data.List (intersperse)
import Data.IORef
import Data.Maybe

features :: [String] -> Set String -> Set String
features labels classes =
  Set.fromList labels `Set.union` classes

-- prop_noNewFeatures :: Testable prop => Set String -> prop -> Property
-- prop_noNewFeatures feats prop =
--   mapResult f prop
--   where
--     f res =
--       case ok res of
--         Just True
--           | not (features (P.labels res) (Set.fromList (P.classes res)) `Set.isSubsetOf` feats) ->
--             res{ok = Just False, P.reason = "New feature found"}
--         _ -> res

-- | Given a property, which must use 'label', 'collect', 'classify' or 'cover'
-- to associate labels with test cases, find an example test case for each possible label.
-- The example test cases are minimised using shrinking.
--
-- For example, suppose we test @'Data.List.delete' x xs@ and record the number
-- of times that @x@ occurs in @xs@:
--
-- > prop_delete :: Int -> [Int] -> Property
-- > prop_delete x xs =
-- >   classify (count x xs == 0) "count x xs == 0" $
-- >   classify (count x xs == 1) "count x xs == 1" $
-- >   classify (count x xs >= 2) "count x xs >= 2" $
-- >   counterexample (show (delete x xs)) $
-- >   count x (delete x xs) == max 0 (count x xs-1)
-- >   where count x xs = length (filter (== x) xs)
--
-- 'labelledExamples' generates three example test cases, one for each label:
-- 
-- >>> labelledExamples prop_delete
-- *** Found example of count x xs == 0
-- 0
-- []
-- []
-- <BLANKLINE>
-- *** Found example of count x xs == 1
-- 0
-- [0]
-- []
-- <BLANKLINE>
-- *** Found example of count x xs >= 2
-- 5
-- [5,5]
-- [5]
-- <BLANKLINE>
-- +++ OK, passed 100 tests:
-- 78% count x xs == 0
-- 21% count x xs == 1
--  1% count x xs >= 2


labelledExamples :: Testable prop => prop -> IO ()
labelledExamples prop = labelledExamplesWith stdArgs prop

-- | A variant of 'labelledExamples' that takes test arguments.
labelledExamplesWith :: Testable prop => Args -> prop -> IO ()
labelledExamplesWith args prop = labelledExamplesWithResult args prop >> return ()

-- | A variant of 'labelledExamples' that returns a result.
labelledExamplesResult :: Testable prop => prop -> IO Result
labelledExamplesResult prop = labelledExamplesWithResult stdArgs prop

-- | A variant of 'labelledExamples' that takes test arguments and returns a result.
labelledExamplesWithResult :: Testable prop => Args -> prop -> IO Result
labelledExamplesWithResult args prop = undefined -- TODO fix this
  -- withState args (\state -> do
  --   let
  --     loop :: Set String -> State -> IO Result
  --     loop feats state = withNullTerminal $ \nullterm -> do
  --       res <- test state{terminal = nullterm} (property (prop_noNewFeatures feats prop))
  --       let feats' = features (failingLabels res) (failingClasses res)
  --       case res of
  --         Failure{reason = "New feature found"} -> do
  --           putLine (terminal state) $
  --             "*** Found example of " ++
  --             concat (intersperse ", " (Set.toList (feats' Set.\\ feats)))
  --           mapM_ (putLine (terminal state)) (failingTestCase res)
  --           putStrLn ""
  --           loop (Set.union feats feats')
  --             state{randomSeed = usedSeed res, computeSize = computeSize state `at0` usedSize res}
  --         _ -> do
  --           out <- terminalOutput nullterm
  --           putStr out
  --           return res
  --     at0 f s 0 0 = s
  --     at0 f s n d = f n d
  --   loop Set.empty state) Nothing 0
