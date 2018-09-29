{-|
The <http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html QuickCheck manual>
gives detailed information about using QuickCheck effectively.
You can also try <https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html>,
a tutorial written by a user of QuickCheck.

To start using QuickCheck, write down your property as a function returning @Bool@.
For example, to check that reversing a list twice gives back the same list you can write:

@
import Test.QuickCheck

prop_reverse :: [Int] -> Bool
prop_reverse xs = reverse (reverse xs) == xs
@

You can then use QuickCheck to test @prop_reverse@ on 100 random lists:

>>> quickCheck prop_reverse
+++ OK, passed 100 tests.

To run more tests you can use the 'withMaxSuccess' combinator:

>>> quickCheck (withMaxSuccess 10000 prop_reverse)
+++ OK, passed 10000 tests.

To use QuickCheck on your own data types you will need to write 'Arbitrary'
instances for those types. See the
<http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html QuickCheck manual> for
details about how to do that.
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
  , isSuccess
    -- ** Running tests verbosely
  , verboseCheck
  , verboseCheckWith
  , verboseCheckWithResult
  , verboseCheckResult
#ifndef NO_TEMPLATE_HASKELL
    -- ** Testing all properties in a module

    -- | These functions test all properties in the current module, using
    -- Template Haskell. You need to have a @{-\# LANGUAGE TemplateHaskell \#-}@
    -- pragma in your module for any of these to work.
  , quickCheckAll
  , verboseCheckAll
  , forAllProperties
  , allProperties
    -- ** Testing polymorphic properties
  , polyQuickCheck
  , polyVerboseCheck
  , monomorphic
#endif

    -- * The 'Arbitrary' typeclass: generation of random values
  , Arbitrary(..)
    -- ** Helper functions for implementing 'shrink'
#ifndef NO_GENERICS
  , genericShrink
  , subterms
  , recursivelyShrink
#endif
  , shrinkNothing
  , shrinkList
  , shrinkMap
  , shrinkMapBy
  , shrinkIntegral
  , shrinkRealFrac
  , shrinkDecimal

    -- ** Lifting of 'Arbitrary' to unary and binary type constructors
  , Arbitrary1(..)
  , arbitrary1
  , shrink1
  , Arbitrary2(..)
  , arbitrary2
  , shrink2

    -- * The 'Gen' monad: combinators for building random generators
  , Gen
    -- ** Generator combinators
  , choose
  , oneof
  , frequency
  , elements
  , growingElements
  , sized
  , getSize
  , resize
  , scale
  , suchThat
  , suchThatMap
  , suchThatMaybe
  , applyArbitrary2
  , applyArbitrary3
  , applyArbitrary4
    -- ** Generators for lists
  , listOf
  , listOf1
  , vectorOf
  , vector
  , infiniteListOf
  , infiniteList
  , shuffle
  , sublistOf
  , orderedList
    -- ** Generators for particular types
  , arbitrarySizedIntegral
  , arbitrarySizedNatural
  , arbitrarySizedFractional
  , arbitrarySizedBoundedIntegral
  , arbitraryBoundedIntegral
  , arbitraryBoundedRandom
  , arbitraryBoundedEnum
  , arbitraryUnicodeChar
  , arbitraryASCIIChar
  , arbitraryPrintableChar
    -- ** Running generators
  , generate
    -- ** Debugging generators
  , sample
  , sample'

    -- * The 'Function' typeclass: generation of random shrinkable, showable functions

    -- | Example of use:
    --
    -- >>> :{
    -- >>> let prop :: Fun String Integer -> Bool
    -- >>>     prop (Fun _ f) = f "monkey" == f "banana" || f "banana" == f "elephant"
    -- >>> :}
    -- >>> quickCheck prop
    -- *** Failed! Falsifiable (after 3 tests and 134 shrinks):
    -- {"elephant"->1, "monkey"->1, _->0}
    --
    -- To generate random values of type @'Fun' a b@,
    -- you must have an instance @'Function' a@.
    -- If your type has a 'Show' instance, you can use 'functionShow' to write the instance; otherwise,
    -- use 'functionMap' to give a bijection between your type and a type that is already an instance of 'Function'.
    -- See the @'Function' [a]@ instance for an example of the latter.
    --
    -- For more information, see the paper \"Shrinking and showing functions\" by Koen Claessen.
  , Fun (..)
  , applyFun
  , applyFun2
  , applyFun3
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 708
  , pattern Fn
  , pattern Fn2
  , pattern Fn3
#endif
  , Function (..)
  , functionMap
  , functionShow
  , functionIntegral
  , functionRealFrac
  , functionBoundedEnum

    -- * The 'CoArbitrary' typeclass: generation of functions the old-fashioned way
  , CoArbitrary(..)
#ifndef NO_GENERICS
  , genericCoarbitrary
#endif
  , variant
  , coarbitraryIntegral
  , coarbitraryReal
  , coarbitraryShow
  , coarbitraryEnum
  , (><)

    -- * Type-level modifiers for changing generator behavior

    -- | These types do things such as restricting the kind of test data that can be generated.
    -- They can be pattern-matched on in properties as a stylistic
    -- alternative to using explicit quantification.
    --
    -- Examples:
    --
    -- @
    -- -- Functions cannot be shown (but see 'Function')
    -- prop_TakeDropWhile ('Blind' p) (xs :: ['A']) =
    --   takeWhile p xs ++ dropWhile p xs == xs
    -- @
    --
    -- @
    -- prop_TakeDrop ('NonNegative' n) (xs :: ['A']) =
    --   take n xs ++ drop n xs == xs
    -- @
    --
    -- @
    -- -- cycle does not work for empty lists
    -- prop_Cycle ('NonNegative' n) ('NonEmpty' (xs :: ['A'])) =
    --   take n (cycle xs) == take n (xs ++ cycle xs)
    -- @
    --
    -- @
    -- -- Instead of 'forAll' 'orderedList'
    -- prop_Sort ('Ordered' (xs :: ['OrdA'])) =
    --   sort xs == xs
    -- @
  , Blind(..)
  , Fixed(..)
  , OrderedList(..)
  , NonEmptyList(..)
  , InfiniteList(..)
  , SortedList(..)
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
  , ASCIIString(..)
  , UnicodeString(..)
  , PrintableString(..)

    -- * Property combinators
  , Property, Testable(..)
  , forAll
  , forAllShrink
  , forAllShow
  , forAllShrinkShow
  , forAllBlind
  , forAllShrinkBlind
  , shrinking
  , (==>)
  , Discard(..)
  , discard
  , (===)
  , (=/=)
#ifndef NO_DEEPSEQ
  , total
#endif
  , ioProperty
  , idempotentIOProperty
    -- ** Controlling property execution
  , verbose
  , verboseShrinking
  , noShrinking
  , withMaxSuccess
  , within
  , once
  , again
  , mapSize
    -- ** Conjunction and disjunction
  , (.&.)
  , (.&&.)
  , conjoin
  , (.||.)
  , disjoin
    -- ** What to do on failure
  , counterexample
  , printTestCase
  , whenFail
  , whenFail'
  , expectFailure
    -- * Analysing test case distribution
  , label
  , collect
  , classify
  , tabulate
    -- ** Checking test case distribution
  , cover
  , coverTable
  , checkCoverage
  , checkCoverageWith
  , Confidence(..)
  , stdConfidence
    -- ** Generating example test cases
  , labelledExamples
  , labelledExamplesWith
  , labelledExamplesWithResult
  , labelledExamplesResult
  )
 where

--------------------------------------------------------------------------
-- imports

import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Modifiers
import Test.QuickCheck.Property hiding ( Result(..) )
import Test.QuickCheck.Test
import Test.QuickCheck.Exception
import Test.QuickCheck.Function
import Test.QuickCheck.Features
import Test.QuickCheck.State
#ifndef NO_TEMPLATE_HASKELL
import Test.QuickCheck.All
#endif

--------------------------------------------------------------------------
-- the end.
