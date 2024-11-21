import Test.QuickCheck
import Test.QuickCheck.Monadic
import Control.Monad
import System.Exit

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
  r <- quickCheckResult $ within 10000 $ forAll arbitrary prop_tabulateALot
  unless (isSuccess r) exitFailure
