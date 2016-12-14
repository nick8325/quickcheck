-- | A Rose 'Tree' based combinators to write generators and shrinkers
-- simultaenously.
--
-- >>> let reven = fmap (*2) rarbitrary
-- >>> quickCheck $ rForAll reven $ \x -> x < 4
-- *** Failed! Falsifiable (after 6 tests and 1 shrink):
-- 4
--
-- See <http://hypothesis.works/articles/integrated-shrinking/>
module Test.QuickCheck.Rose
  (
  -- * Generators
    RGen (..)
  , rarbitrary
  , roneof
  , roneof'
  , rsized
  , rscale
  -- * Properties
  , rForAll
  )
 where

import Control.Applicative
import Control.Monad (join)
import Data.List (inits)
import Data.Traversable (traverse)
import Data.Tree (Tree (..))
import Test.QuickCheck
import Test.QuickCheck.Property (Property (..))

-- | A generator for values of type @a@ with built-in shrinking.
--
-- /Note:/ you should use applicative combinators (or @ApplicativeDo@),
-- as much as possible to get quality shrinking.
newtype RGen a = RGen { runRGen :: Gen (Tree a) }

instance Functor RGen where
    fmap f (RGen g) = RGen (fmap (fmap f) g)

instance Applicative RGen where
    pure = RGen . pure . pure
    RGen f <*> RGen x = RGen $ (<*>) <$> f <*> x

instance Monad RGen where
    return = pure
    RGen g >>= f = RGen $ do
        tree  <- g
        tree' <- traverse (runRGen . f) tree
        pure $ join tree'

-- | Create 'RGen' from plain old 'Arbitrary'.
rarbitrary :: Arbitrary a => RGen a
rarbitrary = RGen $ fmap mkTree arbitrary
  where
    mkTree x = Node x (map mkTree (shrink x))

rsized :: (Int -> RGen a) -> RGen a
rsized f = RGen $ sized (runRGen . f)

rscale :: (Int -> Int) -> RGen a -> RGen a
rscale f (RGen g) = RGen (scale f g)

-- | Randomly uses one of the given generators. The input list must be non-empty.
roneof :: [RGen a] -> RGen a
roneof = RGen . oneof . map runRGen

-- | Variant of 'roneof' which generates uses all generators, so results
-- are shrinked to previous ones in the list.
--
-- This trades generation speed for shrinking quality.
roneof' :: [RGen a] -> RGen a
roneof' rgens = RGen $ do
    trees <- traverse runRGen rgens
    let trees' = map unsnoc $ tail $ inits trees
    elements $ map combineTree trees'

  where
    -- unsafe unsnoc
    unsnoc :: [a] -> ([a], a)
    unsnoc []     = error "unsnoc: empty list"
    unsnoc [x]    = ([], x)
    unsnoc (x:xs) = case unsnoc xs of
        ~(ys, z) -> (x : ys, z)

    combineTree (prev, Node root forest) = Node root (forest ++ prev)


-- | Explicit universal quantification: uses an explicitly given test case generator.
--
-- Compared to 'forAll', this version will automatically shrink the results too.
rForAll :: (Show a, Testable prop) => RGen a => (a -> prop) -> Property
rForAll (RGen rgen) pf = again $ MkProperty $ do
    tree <- rgen
    unProperty $ shrinking subForest tree $ \tree' ->
        let x = rootLabel tree'
        in counterexample (show x) (pf x)
