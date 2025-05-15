import Test.QuickCheck
import Test.QuickCheck.Monadic

main = quickCheck prop_test_withProgress

prop_test_withProgress :: Property
prop_test_withProgress =
    forAll (arbitrary :: Gen Int) $ \n ->
        withProgress (\p -> putStrLn $ "executed tests: " ++ show (numpassed p))
      $ n == n