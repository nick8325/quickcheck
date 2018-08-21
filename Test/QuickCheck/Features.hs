module Test.QuickCheck.Features where

import Test.QuickCheck.Property hiding (Result, reason)
import qualified Test.QuickCheck.Property as P
import Test.QuickCheck.Test
import Test.QuickCheck.Gen
import Test.QuickCheck.State
import Test.QuickCheck.Text
import qualified Data.Set as Set
import Data.Set(Set)
import Data.List
import Data.IORef

prop_noNewFeatures :: Testable prop => Set String -> prop -> Property
prop_noNewFeatures features prop =
  mapResult f prop
  where
    f res =
      case ok res of
        Just True
          | not (Set.fromList (P.labels res) `Set.isSubsetOf` features) ->
            res{ok = Just False, P.reason = "New feature found"}
        _ -> res

collectFeatures :: Testable prop => prop -> IO ()
collectFeatures prop = collectFeaturesWith stdArgs prop

collectFeaturesWith :: Testable prop => Args -> prop -> IO ()
collectFeaturesWith args prop = collectFeaturesWithResult args prop >> return ()

collectFeaturesResult :: Testable prop => prop -> IO Result
collectFeaturesResult prop = collectFeaturesWithResult stdArgs prop

collectFeaturesWithResult :: Testable prop => Args -> prop -> IO Result
collectFeaturesWithResult args prop =
  withState args $ \state -> do
    let
      loop feats state = withNullTerminal $ \nullterm -> do
        res <- test state{terminal = nullterm} (property (prop_noNewFeatures feats prop))
        case res of
          Failure{reason = "New feature found"} -> do
            putLine (terminal state) $
              "*** Found new test case exercising feature " ++ 
              intercalate ", " (Set.toList (features res Set.\\ feats))
            mapM_ (putLine (terminal state)) (failingTestCase res)
            putStrLn ""
            loop (Set.union feats (features res))
              state{randomSeed = usedSeed res, computeSize = computeSize state `at0` usedSize res}
          _ -> do
            out <- terminalOutput nullterm
            putStr out
            return res
      at0 f s 0 0 = s
      at0 f s n d = f n d
    loop Set.empty state
