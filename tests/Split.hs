import Test.QuickCheck
import Test.QuickCheck.Random
import Data.List

-- This type allows us to run integerVariant and get a list of bits out.
newtype Splits = Splits { unSplits :: [Bool] } deriving (Eq, Ord, Show)

instance Splittable Splits where
  left (Splits xs) = Splits (xs ++ [False])
  right (Splits xs) = Splits (xs ++ [True])

-- Check that integerVariant gives a prefix-free code,
-- i.e., if m /= n then integerVariant m is not a prefix of integerVariant n.
prop_split_prefix :: Property
prop_split_prefix =
  once $ forAllShrink (return [-10000..10000]) shrink $ \ns ->
    map head (group (sort ns)) == sort ns ==> -- no duplicates
    let
      codes :: [Splits]
      codes = sort [integerVariant n (Splits []) | n <- ns]

      ok (Splits xs) (Splits ys) = not (xs `isPrefixOf` ys)
    in
      -- After sorting, any prefix will end up immediately before
      -- one of its suffixes
      and (zipWith ok codes (drop 1 codes))

main = do Success{} <- quickCheckResult prop_split_prefix; return ()
