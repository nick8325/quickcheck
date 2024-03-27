-- Miscellaneous tests.

{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck
import Test.QuickCheck.Random
import Data.Map

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

return []
main = do
  True <- $quickCheckAll
  Success{classes=cls} <- quickCheckResult $ classify False "A" $ classify True "B" True
  [("A",0),("B",100)] <- return $ toList cls
  return ()
