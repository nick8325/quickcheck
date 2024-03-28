-- Miscellaneous tests.

{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck
import Test.QuickCheck.Random
import Data.Map
import Control.Monad

prop_verbose :: Blind (Int -> Int -> Bool) -> Property
prop_verbose (Blind p) =
  forAll (mkQCGen <$> arbitrary) $ \g ->
  ioProperty $ do
    res1 <- quickCheckWithResult stdArgs{replay = Just (g, 100), chatty = False} p
    res2 <- quickCheckWithResult stdArgs{replay = Just (g, 100), chatty = False} (verbose p)
    return $
      numTests res1 === numTests res2 .&&.
      failingTestCase res1 === failingTestCase res2

prop_failingTestCase :: Blind (Int -> Int -> Int -> Bool) -> Property
prop_failingTestCase (Blind p) = ioProperty $ do
  res <- quickCheckWithResult stdArgs{chatty = False} p
  let [x, y, z] = failingTestCase res
  return (not (p (read x) (read y) (read z)))

prop_maxSize :: Property
prop_maxSize = withMaxSize 10 (forAll (arbitrary :: Gen Int) $ \ x -> abs x < 10)

prop_cover :: Property
prop_cover = withMaxSuccess 1000
           $ checkCoverage
           $ forAll (arbitrary :: Gen Int)
           $ \ x -> cover 5 (x > 0) "positive" True

-- Issue #382
prop_discardCoverage :: Property
prop_discardCoverage = checkCoverage $ forAll (sized $ \ n -> pure n) $ \ x -> cover 10 True "label" $ x /= 99 ==> True

return []
main = do
  True <- $quickCheckAll
  Success{classes=cls} <- quickCheckResult $ classify False "A" $ classify True "B" True
  [("A",0),("B",100)] <- return $ toList cls
  Success{numTests=1000} <- quickCheckResult prop_cover
  forM_ [const discard, const [discard], \ x -> discard : shrink x] $ \ shr -> do
    Failure{reason="Falsified"} <- quickCheckResult $ forAllShrink arbitrary shr (odd :: Int -> Bool)
    return ()
  return ()
