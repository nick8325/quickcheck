{-| For further information see the <http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html QuickCheck manual>.

To use QuickCheck to check a property, first define a function
expressing that property (functions expressing properties under test
tend to be prefixed with @prop_@). Testing that @n + m = m + n@ holds
for @Integer@s one might write:

@
import Test.QuickCheck

prop_commutativeAdd :: Integer -> Integer -> Bool
prop_commutativeAdd n m = n + m == m + n
@

and testing:

>>> quickCheck prop_commutativeAdd
+++ OK, passed 100 tests.

which tests @prop_commutativeAdd@ on 100 random @(Integer, Integer)@ pairs.

'verboseCheck' can be used to see the actual values generated:

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
increased by updating the 'stdArgs' record:

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
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE PatternSynonyms #-}
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
  , scale
  , suchThat
  , suchThatMaybe
  , listOf
  , listOf1
  , vectorOf
  , infiniteListOf
  , shuffle
  , sublistOf
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
  , arbitrarySizedNatural
  , arbitrarySizedFractional
  , arbitrarySizedBoundedIntegral
  , arbitraryBoundedIntegral
  , arbitraryBoundedRandom
  , arbitraryBoundedEnum
    -- ** Helper functions for implementing shrink
#ifndef NO_GENERICS
  , genericArbitrary
  , genericCoarbitrary
  , genericShrink
  , subterms
  , recursivelyShrink
#endif
  , shrinkNothing
  , shrinkList
  , shrinkIntegral
  , shrinkRealFrac
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

    -- ** Functions
  , Fun
  , appFun
  , Fun2
  , appFun2
  , Fun3
  , appFun3
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 708
  , pattern Fn
  , pattern Fn2
  , pattern Fn3
#endif
  , Function (..)
  , functionMap

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
  , again
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
import Test.QuickCheck.Function
#ifndef NO_TEMPLATE_HASKELL
import Test.QuickCheck.All
#endif

--------------------------------------------------------------------------
-- the end.
