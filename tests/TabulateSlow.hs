import Test.QuickCheck
import Test.QuickCheck.Monadic

prop_tabulateALot :: Int -> Property
prop_tabulateALot x =
  tabulates 1000
  where
    tabulates 0 = x === x
    tabulates n =
      tabulate "World" ["Hello"] $
      tabulate "Hello" (["World" | even n] ++ ["There" | odd n]) $
      tabulates (n - 1)

main = do
  quickCheck $ forAll arbitrary prop_tabulateALot
