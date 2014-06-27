{-| For further information see the <http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html QuickCheck manual>.

To use QuickCheck to check a property, first define a function
expressing that property (functions expressing properties under test
end to be prefixed with @prop_@). Testing that @n + m = m + n@ holds
for @Integer@s one might write:

@
    import Test.QuickCheck

    prop_commutativeAdd :: Integer -> Integer -> Bool
    prop_commutativeAdd n m = n + m == m + n
@

and testing it with:

>>> quickcheck prop_commutativeAdd
+++ OK, passed 100 tests.

which generates 200 @Integer@s and checks that @prop_commutativeadd@
holds for them.

To see the actual values generated 'verboseCheck' can be used:

>>> verboseCheck prop_commutativeAdd
Passed:
0
0
  …98 tests omitted…
Passed:
-68
6
+++ OK, passed 100 tests.

and if more than 100 tests are needed the number of tests can be
increased as such:

>>> quickCheckWith stdArgs { maxSuccess = 500 } prop_commutativeAdd
+++ OK, passed 500 tests.

To let QuickCheck generate values of your own data type an 'Arbitrary'
instance must be defined:

@
    data Point = MkPoint Int Int deriving Eq
    
    instance Arbitrary Point where
      arbitrary = do
        x <- 'arbitrary'
        y <- arbitrary
        return (MkPoint x y)
    
    swapPoint :: Point -> Point
    swapPoint (MkPoint x y) = MkPoint y x

    -- swapPoint . swapPoint = id
    prop_swapInvolution point = swapPoint (swapPoint point) == point
@

>>> quickCheck prop_swapInvolution
+++ OK, passed 100 tests.

See "Test.QuickCheck.Function" for generating random shrinkable,
showable functions used for testing higher-order functions and
"Test.QuickCheck.Monadic" for testing impure or monadic code
(e.g. effectful code in 'IO').

-}
{-# LANGUAGE CPP #-}
#ifndef NO_SAFE_HASKELL
{-# LANGUAGE Safe #-}
#endif
module Test.QuickCheck
  (
    -- * Running tests
    quickCheck
  , Args(..), Result(..)
  , stdArgs
  , quickCheckWith
  , quickCheckWithResult
  , quickCheckResult
    -- ** Running tests verbosely
  , verboseCheck
  , verboseCheckWith
  , verboseCheckWithResult
  , verboseCheckResult
#ifndef NO_TEMPLATE_HASKELL
    -- ** Testing all properties in a module
  , quickCheckAll
  , verboseCheckAll
  , forAllProperties
    -- ** Testing polymorphic properties
  , polyQuickCheck
  , polyVerboseCheck
  , monomorphic
#endif

    -- * Random generation
  , Gen
    -- ** Generator combinators
  , choose
  , oneof
  , frequency
  , elements
  , growingElements
  , sized
  , resize
  , suchThat
  , suchThatMaybe
  , listOf
  , listOf1
  , vectorOf
  , infiniteListOf
    -- ** Generators which use Arbitrary
  , vector
  , orderedList
  , infiniteList
    -- ** Running a generator
  , generate
    -- ** Generator debugging
  , sample
  , sample'

    -- * Arbitrary and CoArbitrary classes
  , Arbitrary(..)
  , CoArbitrary(..)

    -- ** Helper functions for implementing arbitrary
  , arbitrarySizedIntegral
  , arbitrarySizedFractional
  , arbitrarySizedBoundedIntegral
  , arbitraryBoundedIntegral
  , arbitraryBoundedRandom
  , arbitraryBoundedEnum
    -- ** Helper functions for implementing shrink
#ifndef NO_GENERICS
  , genericShrink
  , subterms
  , recursivelyShrink
#endif
  , shrinkNothing
  , shrinkList
  , shrinkIntegral
  , shrinkRealFrac
  , shrinkRealFracToInteger
    -- ** Helper functions for implementing coarbitrary
  , variant
  , coarbitraryIntegral
  , coarbitraryReal
  , coarbitraryShow
  , coarbitraryEnum
  , (><)

    -- ** Type-level modifiers for changing generator behavior
  , Blind(..)
  , Fixed(..)
  , OrderedList(..)
  , NonEmptyList(..)
  , Positive(..)
  , NonZero(..)
  , NonNegative(..)
  , Large(..)
  , Small(..)
  , Smart(..)
  , Shrink2(..)
#ifndef NO_MULTI_PARAM_TYPE_CLASSES
  , Shrinking(..)
  , ShrinkState(..)
#endif

    -- * Properties
  , Property, Testable(..)
    -- ** Property combinators
  , forAll
  , forAllShrink
  , shrinking
  , (==>)
  , (===)
  , ioProperty
    -- *** Controlling property execution
  , verbose
  , once
  , within
  , noShrinking
    -- *** Conjunction and disjunction
  , (.&.)
  , (.&&.)
  , conjoin
  , (.||.)
  , disjoin
    -- *** What to do on failure
  , counterexample
  , printTestCase
  , whenFail
  , whenFail'
  , expectFailure
    -- *** Analysing test distribution
  , label
  , collect
  , classify
  , cover
    -- *** Miscellaneous
  , Discard(..)
  , discard
  , mapSize
  )
 where

--------------------------------------------------------------------------
-- imports

import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Modifiers
import Test.QuickCheck.Property hiding ( Result(..) )
import Test.QuickCheck.Test
import Test.QuickCheck.Text
import Test.QuickCheck.Exception
#ifndef NO_TEMPLATE_HASKELL
import Test.QuickCheck.All
#endif

--------------------------------------------------------------------------
-- the end.
