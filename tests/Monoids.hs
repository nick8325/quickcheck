{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell  #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Main (main) where

import Data.List.NonEmpty
import Data.Semigroup (Semigroup (..))

import Test.QuickCheck

instance Arbitrary Every where
    arbitrary = oneof [ pure $ Every True
                      , pure $ Every False
                      , pure $ Every (counterexample "False" False)
                      , pure $ Every (counterexample "True" True)
                      , pure $ Every (ioProperty (return True))
                      , pure $ Every (ioProperty (return False))
                      , pure $ Every (checkCoverage $ cover 100 True "" True)
                      , pure $ Every (checkCoverage $ cover 100 True "" False)
                      , pure $ Every (checkCoverage $ cover 100 False "" False)
                      ]


instance Arbitrary Some where
    arbitrary = oneof [ pure $ Some True
                      , pure $ Some False
                      , pure $ Some (counterexample "False" False)
                      , pure $ Some (counterexample "True" True)
                      , pure $ Some (ioProperty (return True))
                      , pure $ Some (ioProperty (return False))
                      , pure $ Some (checkCoverage $ cover 100 True  "" True)
                      , pure $ Some (checkCoverage $ cover 100 True  "" False)
                      , pure $ Some (checkCoverage $ cover 100 False "" True)
                      , pure $ Some (checkCoverage $ cover 100 False "" False)
                      ]


newtype Fail a = Fail a

instance Arbitrary (Fail Every) where
    arbitrary = oneof [ Fail <$> (arbitrary :: Gen Every)
                      , pure $ Fail $ Every (checkCoverage $ cover 100 False "" True)
                      ]


check_associative_law :: (Testable p, Semigroup p) => Blind p -> Blind p -> Blind p -> Property
check_associative_law (Blind a) (Blind b) (Blind c) = ioProperty $ do
    x <- isSuccess <$> quickCheckWithResult args (a <> (b <> c))
    y <- isSuccess <$> quickCheckWithResult args ((a <> b) <> c)
    return (x == y)


#ifndef NO_SEMIGROUP_SUPERCLASS
check_unit_law :: (Testable p, Monoid p) => Blind p -> Property
#else
check_unit_law :: (Testable p, Monoid p, Semigroup p) => Blind p -> Property
#endif
check_unit_law (Blind a) = ioProperty $ do
    x <- isSuccess <$> quickCheckWithResult args (a <> mempty)
    y <- isSuccess <$> quickCheckWithResult args (mempty <> a)
    z <- isSuccess <$> quickCheckWithResult args a
    return (x == y .&&. y == z)


check_sconcat_law :: (Testable p, Semigroup p) => Blind p -> Blind p -> Property
check_sconcat_law (Blind a) (Blind b) = ioProperty $ do
    x <- isSuccess <$> quickCheckWithResult args (sconcat $ a :| [b])
    y <- isSuccess <$> quickCheckWithResult args (a <> b)
    return (x == y)


#ifndef NO_SEMIGROUP_SUPERCLASS
check_mconcat_law :: (Testable p, Monoid p) => Blind p -> Blind p -> Property
#else
check_mconcat_law :: (Testable p, Monoid p, Semigroup p) => Blind p -> Blind p -> Property
#endif
check_mconcat_law (Blind a) (Blind b) = ioProperty $ do
    x <- isSuccess <$> quickCheckWithResult args (mconcat [a, b])
    y <- isSuccess <$> quickCheckWithResult args (a <> b)
    return (x == y)


--
-- Auxiliary definitions
--

args :: Args
args = stdArgs { chatty = False, maxShrinks = 0 }

--
-- Properties
--

prop_every_associative :: Blind Every -> Blind Every -> Blind Every -> Property
prop_every_associative = check_associative_law

prop_every_unit :: Blind Every -> Property
prop_every_unit = check_unit_law

prop_every_unit_fail :: Blind (Fail Every) -> Property
prop_every_unit_fail (Blind (Fail a)) =
    expectFailure $ check_unit_law (Blind a)

prop_every_sconcat_law :: Blind Every -> Blind Every -> Property
prop_every_sconcat_law = check_sconcat_law

prop_every_mconcat_law :: Blind Every -> Blind Every -> Property
prop_every_mconcat_law = check_mconcat_law

prop_some_associative :: Blind Some -> Blind Some -> Blind Some -> Property
prop_some_associative = check_associative_law

prop_some_unit :: Blind Some -> Property
prop_some_unit = check_unit_law

prop_some_sconcat_law :: Blind Some -> Blind Some -> Property
prop_some_sconcat_law = check_sconcat_law

prop_some_mconcat_law :: Blind Some -> Blind Some -> Property
prop_some_mconcat_law = check_mconcat_law

return []
main = $quickCheckAll

