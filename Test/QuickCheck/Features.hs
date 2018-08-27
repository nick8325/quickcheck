module Test.QuickCheck.Features where

import Test.QuickCheck.Property hiding (Result, reason)
import qualified Test.QuickCheck.Property as P
import Test.QuickCheck.Test
import Test.QuickCheck.Gen
import Test.QuickCheck.State
import Test.QuickCheck.Text
import qualified Data.Set as Set
import Data.Set(Set)
import qualified Data.Map.Strict as Map
import Data.List
import Data.IORef

lookupFeatures :: Maybe String -> Result -> Set String
lookupFeatures Nothing res = Set.fromList (failingLabels res)
lookupFeatures (Just table) res = Map.keysSet (Map.findWithDefault Map.empty table (failingTables res))

lookupPropFeatures :: Maybe String -> P.Result -> Set String
lookupPropFeatures Nothing res = Set.fromList (P.labels res)
lookupPropFeatures (Just table) res = Map.keysSet (Map.findWithDefault Map.empty table (P.tables res))

prop_noNewFeatures :: Testable prop => Maybe String -> Set String -> prop -> Property
prop_noNewFeatures mtable features prop =
  mapResult f prop
  where
    f res =
      case ok res of
        Just True
          | not (lookupPropFeatures mtable res `Set.isSubsetOf` features) ->
            res{ok = Just False, P.reason = "New feature found"}
        _ -> res

labelledExamples :: Testable prop => Maybe String -> prop -> IO ()
labelledExamples mtable prop = labelledExamplesWith stdArgs mtable prop

labelledExamplesWith :: Testable prop => Args -> Maybe String -> prop -> IO ()
labelledExamplesWith args mtable prop = labelledExamplesWithResult args mtable prop >> return ()

labelledExamplesResult :: Testable prop => Maybe String -> prop -> IO Result
labelledExamplesResult mtable prop = labelledExamplesWithResult stdArgs mtable prop

labelledExamplesWithResult :: Testable prop => Args -> Maybe String -> prop -> IO Result
labelledExamplesWithResult args mtable prop =
  withState args $ \state -> do
    let
      loop :: Set String -> State -> IO Result
      loop feats state = withNullTerminal $ \nullterm -> do
        res <- test state{terminal = nullterm} (property (prop_noNewFeatures mtable feats prop))
        case res of
          Failure{reason = "New feature found"} -> do
            putLine (terminal state) $
              "*** Found new test case exercising feature " ++ 
              intercalate ", " (Set.toList (lookupFeatures mtable res Set.\\ feats))
            mapM_ (putLine (terminal state)) (failingTestCase res)
            putStrLn ""
            loop (Set.union feats (lookupFeatures mtable res))
              state{randomSeed = usedSeed res, computeSize = computeSize state `at0` usedSize res}
          _ -> do
            out <- terminalOutput nullterm
            putStr out
            return res
      at0 f s 0 0 = s
      at0 f s n d = f n d
    loop Set.empty state
